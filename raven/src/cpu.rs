use crate::csr::{Mxl, PrivLevel};
use crate::exceptions::*;
use crate::instructions::{ILenGroup, Instruction};
use crate::memory::*;
use crate::registers::*;
use bitbybit::bitfield;

// Sign extends a byte to a double-word
macro_rules! sign_ext_b {
    ($val:expr) => {
        (($val as i8) as u64)
    };
}
pub(crate) use sign_ext_b;

// Sign extends a half-word to a double-word
macro_rules! sign_ext_h {
    ($val:expr) => {
        (($val as i16) as u64)
    };
}
pub(crate) use sign_ext_h;

// Sign extends a word to a double-word
macro_rules! sign_ext_w {
    ($val:expr) => {
        (($val as i32) as u64)
    };
}
pub(crate) use sign_ext_w;

// Zero extends any value to a double-word
macro_rules! zero_ext {
    ($val:expr) => {
        ($val as u64)
    };
}
pub(crate) use zero_ext;

#[derive(Debug)]
pub enum IsaError {
    Invalid,
    BaseNotSupported,
    ExtNotSupported,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BaseIsa {
    RV32I,
    RV64I,
}

#[derive(Clone, Copy, Debug)]
pub enum Extension {
    M,
    A,
    F,
    D,
    C,
    S,
    ZICSR,
    ZIFENCEI,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Ialign {
    I16,
    I32,
}

#[bitfield(u8, default = 0)]
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

#[derive(PartialEq, Clone, Copy)]
pub(crate) enum PrivMode {
    Machine,
    Supervisor,
    User,
}

impl TryFrom<PrivLevel> for PrivMode {
    type Error = ();

    fn try_from(level: PrivLevel) -> Result<Self, ()> {
        match level {
            PrivLevel::Machine => Ok(Self::Machine),
            PrivLevel::Supervisor => Ok(Self::Supervisor),
            PrivLevel::User => Ok(Self::User),
            _ => Err(()),
        }
    }
}

pub struct Cpu {
    pub(crate) base_isa: BaseIsa,
    pub(crate) extensions: Extensions,
    pub(crate) reg: Registers,
    pub(crate) priv_mode: PrivMode,
    pub halted: bool,
    reset_addr: u64,
}

impl Cpu {
    fn parse_isa(&mut self, isa: &str) -> Result<(), IsaError> {
        // At least need 5 characters to identify base ISA
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
            "RV64I" | "RV64E" => self.base_isa = BaseIsa::RV64I,
            _ => return Err(IsaError::BaseNotSupported),
        }

        // Keep track of when we are in a multi-character extension identifier
        let mut in_multi = false;
        let mut multi = String::new();

        for c in isa.chars().skip(5) {
            if in_multi {
                match c {
                    '_' | '.' if multi == "ICSR" => {
                        self.extensions = self.extensions.with_zicsr(true);
                        multi.clear();
                        in_multi = false;
                    }
                    '_' | '.' if multi == "IFENCEI" => {
                        self.extensions = self.extensions.with_zifencei(true);
                        multi.clear();
                        in_multi = false;
                    }
                    '_' | '.' => return Err(IsaError::ExtNotSupported),
                    _ => multi.push(c),
                }
            } else {
                match c {
                    'M' => self.extensions = self.extensions.with_m(true),
                    'A' => self.extensions = self.extensions.with_a(true),
                    'F' => self.extensions = self.extensions.with_f(true).with_zicsr(true),
                    'D' => {
                        self.extensions = self.extensions.with_d(true).with_f(true).with_zicsr(true)
                    }
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
                    '_' | '.' => (), // Pass
                    _ => return Err(IsaError::ExtNotSupported),
                }
            }
        }

        Ok(())
    }

    pub(crate) fn xlen(&self) -> BaseIsa {
        if self.base_isa == BaseIsa::RV64I {
            let xlen = match self.priv_mode {
                PrivMode::Machine => self.reg.csr.misa.misa64().mxl(),
                PrivMode::Supervisor => self.reg.csr.mstatus.mstatus64().sxl(),
                PrivMode::User => self.reg.csr.mstatus.mstatus64().uxl(),
            };

            match xlen {
                Mxl::Xlen32 => BaseIsa::RV32I,
                _ => BaseIsa::RV64I,
            }
        } else {
            BaseIsa::RV32I
        }
    }

    pub(crate) fn ialign(&self) -> Ialign {
        if self.ext_supported(Extension::C) {
            Ialign::I16
        } else {
            Ialign::I32
        }
    }

    pub(crate) fn ext_supported(&self, ext: Extension) -> bool {
        let mext = match self.base_isa {
            BaseIsa::RV32I => self.reg.csr.misa.misa32().extensions(),
            BaseIsa::RV64I => self.reg.csr.misa.misa64().extensions(),
        };

        match ext {
            Extension::M => mext.m(),
            Extension::A => mext.a(),
            Extension::F => mext.f(),
            Extension::D => mext.d(),
            Extension::C => mext.c(),
            Extension::S => mext.s(),

            // These cannot be toggled in machine mode
            Extension::ZICSR => self.extensions.zicsr(),
            Extension::ZIFENCEI => self.extensions.zicsr(),
        }
    }

    pub(crate) fn _interrupts_enabled(&self) -> bool {
        todo!();
    }

    pub(crate) fn endian(&self) -> Endian {
        let be = match (self.base_isa, self.priv_mode) {
            (BaseIsa::RV32I, PrivMode::Machine) => self.reg.csr.mstatus.mstatus32h().mbe(),
            (BaseIsa::RV32I, PrivMode::Supervisor) => self.reg.csr.mstatus.mstatus32h().sbe(),
            (BaseIsa::RV32I, PrivMode::User) => {
                self.reg.csr.mstatus.mstatus32l().low_fields().ube()
            }

            (BaseIsa::RV64I, PrivMode::Machine) => self.reg.csr.mstatus.mstatus64().mbe(),
            (BaseIsa::RV64I, PrivMode::Supervisor) => self.reg.csr.mstatus.mstatus64().sbe(),
            (BaseIsa::RV64I, PrivMode::User) => self.reg.csr.mstatus.mstatus64().low_fields().ube(),
        };

        match be {
            false => Endian::Little,
            true => Endian::Big,
        }
    }

    pub(crate) fn loadb(&self, memory: &impl MemoryAccess, addr: u64) -> Result<u8, MemoryError> {
        memory.loadb(addr)
    }

    pub(crate) fn loadh(&self, memory: &impl MemoryAccess, addr: u64) -> Result<u16, MemoryError> {
        memory.loadh(addr, self.endian())
    }

    pub(crate) fn loadw(&self, memory: &impl MemoryAccess, addr: u64) -> Result<u32, MemoryError> {
        memory.loadw(addr, self.endian())
    }

    pub(crate) fn loadd(&self, memory: &impl MemoryAccess, addr: u64) -> Result<u64, MemoryError> {
        memory.loadd(addr, self.endian())
    }

    pub(crate) fn storeb(
        &mut self,
        memory: &mut impl MemoryAccess,
        addr: u64,
        val: u8,
    ) -> Result<(), MemoryError> {
        memory.storeb(addr, val)
    }

    pub(crate) fn storeh(
        &mut self,
        memory: &mut impl MemoryAccess,
        addr: u64,
        val: u16,
    ) -> Result<(), MemoryError> {
        memory.storeh(addr, val, self.endian())
    }

    pub(crate) fn storew(
        &mut self,
        memory: &mut impl MemoryAccess,
        addr: u64,
        val: u32,
    ) -> Result<(), MemoryError> {
        memory.storew(addr, val, self.endian())
    }

    pub(crate) fn stored(
        &mut self,
        memory: &mut impl MemoryAccess,
        addr: u64,
        val: u64,
    ) -> Result<(), MemoryError> {
        memory.stored(addr, val, self.endian())
    }

    pub fn new(isa: &str, reset_addr: u64) -> Result<Self, IsaError> {
        let mut cpu = Self {
            reg: Registers::default(),
            base_isa: BaseIsa::RV32I,
            extensions: Extensions::default(),
            priv_mode: PrivMode::Machine,
            reset_addr,
            halted: false,
        };

        cpu.parse_isa(isa)?;
        cpu.reset();

        Ok(cpu)
    }

    pub fn reset(&mut self) {
        self.halted = false;
        self.priv_mode = PrivMode::Machine;
        self.reg = Registers::default();
        self.reg.pc = self.reset_addr;
        self.reset_csr();
    }

    pub fn step(&mut self, memory: &mut impl MemoryAccess) {
        let instr = match memory.loadw(self.read_pc(), Endian::Little) {
            Ok(w) => w,
            Err(_) => {
                self.trap(Trap::InstructionAccessFault);
                return;
            }
        };
        let instr = Instruction::new_with_raw_value(instr);

        match instr.opcode().ilen() {
            ILenGroup::B32 => self.handle_instr(instr, memory),
            _ => self.handle_instr_c(instr.half(), memory),
        }

        self.update_pc();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_isa_success() {
        let cpu = Cpu::new("RV32I", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));

        let cpu = Cpu::new("RV32E", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));

        let cpu = Cpu::new("RV64I", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));

        let cpu = Cpu::new("RV64E", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));

        let cpu = Cpu::new("RV32IM", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));
        assert!(cpu.extensions.m());

        let cpu = Cpu::new("RV64IA", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));
        assert!(cpu.extensions.a());

        let cpu = Cpu::new("RV32IF", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));
        assert!(cpu.extensions.f() && cpu.extensions.zicsr());

        let cpu = Cpu::new("RV64I_D", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));
        assert!(cpu.extensions.d() && cpu.extensions.f() && cpu.extensions.zicsr());

        let cpu = Cpu::new("RV64IZicsr", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));
        assert!(cpu.extensions.zicsr());

        let cpu = Cpu::new("RV32IZifencei", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));
        assert!(cpu.extensions.zifencei());

        let cpu = Cpu::new("RV64IMZicsr_Zifencei", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV64I));
        assert!(cpu.extensions.zicsr() && cpu.extensions.zifencei() && cpu.extensions.m());

        let cpu = Cpu::new("RV32IG", 0).unwrap();
        assert!(matches!(cpu.base_isa, BaseIsa::RV32I));
        assert!(
            cpu.extensions.m()
                && cpu.extensions.a()
                && cpu.extensions.f()
                && cpu.extensions.d()
                && cpu.extensions.zicsr()
                && cpu.extensions.zifencei()
        );
    }

    #[test]
    fn test_parse_isa_err() {
        assert!(matches!(Cpu::new("ARM", 0), Err(IsaError::Invalid)));
        assert!(matches!(
            Cpu::new("RV128I", 0),
            Err(IsaError::BaseNotSupported)
        ));
        assert!(matches!(
            Cpu::new("RV32IV", 0),
            Err(IsaError::ExtNotSupported)
        ));
        assert!(matches!(
            Cpu::new("RV32IZxxn", 0),
            Err(IsaError::ExtNotSupported)
        ));
    }
}
