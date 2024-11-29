use crate::instr_formats::*;
use crate::memory::*;
use crate::registers::*;
use bitbybit::bitfield;

#[derive(Debug)]
pub enum IsaError {
    Invalid,
    BaseNotSupported,
    ExtNotSupported,
}

pub enum BaseIsa {
    RV32I,
    RV64I,
}

#[bitfield(u8)]
pub(crate) struct Extensions {
    #[bit(0, rw)]
    m: bool,

    #[bit(1, rw)]
    a: bool,

    #[bit(2, rw)]
    f: bool,

    #[bit(3, rw)]
    d: bool,

    #[bit(4, rw)]
    c: bool,

    #[bit(5, rw)]
    s: bool,

    #[bit(6, rw)]
    zicsr: bool,

    #[bit(7, rw)]
    zifencei: bool,
}

pub struct Cpu {
    pub(crate) base_isa: BaseIsa,
    pub(crate) extensions: Extensions,
    pub(crate) reg: Registers,
    pub halted: bool,
    reset_addr: u64,
}

impl Cpu {
    fn handle_instr(&mut self, instr: Instruction, memory: &mut impl MemoryAccess) {
        self.write_pc_next_add(4);

        match instr.opcode().major() {
            MajorGroup::Load => self.handle_load(instr, memory),
            MajorGroup::LoadFP => self.handle_load_fp(instr, memory),
            MajorGroup::Custom0 => self.handle_custom_0(instr),
            MajorGroup::MiscMem => self.handle_misc_mem(instr, memory),
            MajorGroup::OpImm => self.handle_op_imm(instr),
            MajorGroup::AuiPC => self.handle_auipc(instr),
            MajorGroup::OpImm32 => self.handle_op_imm_32(instr),
            MajorGroup::B48 => self.handle_b48(instr),
            MajorGroup::Store => self.handle_store(instr, memory),
            MajorGroup::StoreFP => self.handle_store_fp(instr, memory),
            MajorGroup::Custom1 => self.handle_custom_1(instr),
            MajorGroup::AMO => self.handle_amo(instr, memory),
            MajorGroup::Op => self.handle_op(instr),
            MajorGroup::Lui => self.handle_lui(instr),
            MajorGroup::Op32 => self.handle_op_32(instr),
            MajorGroup::B64 => self.handle_b64(instr),
            MajorGroup::MAdd => self.handle_madd(instr),
            MajorGroup::MSub => self.handle_msub(instr),
            MajorGroup::NMSub => self.handle_nmsub(instr),
            MajorGroup::NMAdd => self.handle_nmadd(instr),
            MajorGroup::OpFP => self.handle_op_fp(instr),
            MajorGroup::OpV => self.handle_op_v(instr),
            MajorGroup::Custom2 => self.handle_custom_2(instr),
            MajorGroup::B48_2 => self.handle_b48_2(instr),
            MajorGroup::Branch => self.handle_branch(instr),
            MajorGroup::Jalr => self.handle_jalr(instr),
            MajorGroup::Reserved => self.handle_reserved(instr),
            MajorGroup::Jal => self.handle_jal(instr),
            MajorGroup::System => self.handle_system(instr),
            MajorGroup::OpVE => self.handle_op_ve(instr),
            MajorGroup::Custom3 => self.handle_custom_3(instr),
            MajorGroup::B80 => self.handle_b80(instr),
        }
    }

    fn handle_instr_c(&mut self, _instr: u16, _memory: &mut impl MemoryAccess) {
        if self.extensions.c() {
            self.write_pc_next_add(2);
            todo!();
        } else {
            // Raise exception
            todo!();
        }
    }

    fn parse_isa(&mut self, isa: &str) -> Result<(), IsaError> {
        // At least need 5 chaarcters to identify base ISA
        if isa.len() < 5 {
            return Err(IsaError::Invalid);
        }

        // Naming strings are case-insensitive
        let mut isa = isa.to_ascii_uppercase();

        // Add arbitrary character to mark end of string to simplify match logic below
        isa.push('.');

        // Identify base ISA (which always appears first in string)
        // The E-variants are strict subsets of the I-variants and are inherently supported
        let base_isa = &isa[..=4];
        match base_isa {
            "RV32I" | "RV32E" => self.base_isa = BaseIsa::RV32I,
            "RV364I" | "RV64E" => self.base_isa = BaseIsa::RV64I,
            _ => return Err(IsaError::BaseNotSupported),
        }

        // Keep track of when we are in a multi-character extension identifier
        let mut in_multi = false;
        let mut multi = String::new();

        for c in isa.chars().skip(5) {
            match c {
                'M' => self.extensions = self.extensions.with_m(true),
                'A' => self.extensions = self.extensions.with_a(true),
                'F' => self.extensions = self.extensions.with_f(true).with_zicsr(true),
                'D' => self.extensions = self.extensions.with_d(true).with_f(true).with_zicsr(true),
                'C' => self.extensions = self.extensions.with_c(true),
                'S' => self.extensions = self.extensions.with_s(true),
                'Z' => in_multi = true,
                'G' => {
                    self.extensions = self
                        .extensions
                        .with_m(true)
                        .with_a(true)
                        .with_f(true)
                        .with_d(true)
                        .with_zicsr(true)
                        .with_zifencei(true)
                }

                '_' | '.' if in_multi && multi == "ICSR" => {
                    self.extensions = self.extensions.with_zicsr(true);
                    multi.clear();
                    in_multi = false;
                }
                '_' | '.' if in_multi && multi == "IFENCEI" => {
                    self.extensions = self.extensions.with_zifencei(true);
                    multi.clear();
                    in_multi = false;
                }

                '_' | '.' if in_multi => return Err(IsaError::ExtNotSupported),
                '_' | '.' => (), // Pass

                _ if in_multi => multi.push(c),
                _ => return Err(IsaError::ExtNotSupported),
            }
        }

        Ok(())
    }

    pub fn new(isa: &str, reset_addr: u64) -> Result<Self, IsaError> {
        let mut cpu = Self {
            base_isa: BaseIsa::RV32I,
            extensions: Extensions::new_with_raw_value(0),
            reg: Registers::default(),
            reset_addr,
            halted: false,
        };

        cpu.parse_isa(isa)?;
        cpu.reset();

        Ok(cpu)
    }

    pub fn reset(&mut self) {
        self.halted = false;
        self.reg = Registers::default();
        self.reg.pc = self.reset_addr;
    }

    pub fn step(&mut self, memory: &mut impl MemoryAccess) {
        let instr = match memory.loadw(self.reg.pc) {
            Ok(w) => w,
            Err(e) => panic!("{:?}", e),
        };
        let instr = Instruction::new_with_raw_value(instr);

        match instr.opcode().ilen() {
            ILenGroup::B32 => self.handle_instr(instr, memory),
            _ => self.handle_instr_c(instr.half(), memory),
        }

        self.update_pc();
    }
}
