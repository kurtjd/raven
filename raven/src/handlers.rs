use arbitrary_int::*;
use softfloat_wrapper::{ExceptionFlags, Float, F32};

use crate::cpu::*;
use crate::exceptions::Trap;
use crate::instructions::*;
use crate::memory::*;

/* Not needed for functionality, but helpful for explicitly
 * showing an opcode should behave as a noop.
 */
macro_rules! nop {
    () => {};
}

impl Cpu {
    /* Just for DRY purposes, since there are quite a few AMO instructions
     * that only differ by the op they perform, which is passed in via closure.
     */
    fn amow<F>(&mut self, src: u64, rd: u8, addr: u64, memory: &mut impl MemoryAccess, op: F)
    where
        F: FnOnce(u32, u32) -> u32,
    {
        if addr % 4 == 0 {
            match self.loadw(memory, addr) {
                Ok(w) => {
                    let res = op(src as u32, w);
                    match self.storew(memory, addr, res) {
                        Ok(_) => self.write_gpr(rd, sign_ext_w!(w)),
                        Err(_) => self.trap(Trap::StoreAccessFault),
                    }
                }
                Err(_) => self.trap(Trap::LoadAccessFault),
            }
        } else {
            self.trap(Trap::LoadAddressMisaligned);
        }
    }

    fn amod<F>(&mut self, src: u64, rd: u8, addr: u64, memory: &mut impl MemoryAccess, op: F)
    where
        F: FnOnce(u64, u64) -> u64,
    {
        if addr % 8 == 0 {
            match self.loadd(memory, addr) {
                Ok(w) => {
                    let res = op(src, w);
                    match self.stored(memory, addr, res) {
                        Ok(_) => self.write_gpr(rd, w),
                        Err(_) => self.trap(Trap::StoreAccessFault),
                    }
                }
                Err(_) => self.trap(Trap::LoadAccessFault),
            }
        } else {
            self.trap(Trap::LoadAddressMisaligned);
        }
    }

    fn update_fflags(&mut self, fflags: &mut ExceptionFlags) {
        fflags.get();
        self.reg.csr.fcsr = self
            .reg
            .csr
            .fcsr
            .with_nx(fflags.is_inexact())
            .with_uf(fflags.is_underflow())
            .with_of(fflags.is_overflow())
            .with_dz(fflags.is_infinite())
            .with_nv(fflags.is_invalid());
    }

    pub(crate) fn handle_instr(&mut self, instr: Instruction, memory: &mut impl MemoryAccess) {
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
            MajorGroup::MAdd => self.handle_ffused(instr),
            MajorGroup::MSub => self.handle_ffused(instr),
            MajorGroup::NMSub => self.handle_ffused(instr),
            MajorGroup::NMAdd => self.handle_ffused(instr),
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

    pub(crate) fn handle_instr_c(&mut self, _instr: u16, _memory: &mut impl MemoryAccess) {
        if self.ialign() == Ialign::I16 {
            self.write_pc_next_add(2);
            todo!();
        } else {
            self.trap(Trap::IllegalInstruction);
        }
    }

    pub(crate) fn handle_load(&mut self, instr: Instruction, memory: &impl MemoryAccess) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();
        let rs = instr.rs1().value();
        let imm = self.expand_imm_i(instr.imm().value());

        // Need to account for the fact that RV32I is limited to 32-bit addr space
        let mut addr = self.read_gpr(rs).wrapping_add(imm);
        if matches!(self.xlen(), BaseIsa::RV32I) {
            addr &= 0xFFFF_FFFF;
        }

        match funct3 {
            funct3::LB => {
                let val = match self.loadb(memory, addr) {
                    Ok(b) => sign_ext_b!(b),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LH => {
                let val = match self.loadh(memory, addr) {
                    Ok(h) => sign_ext_h!(h),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LW => {
                let val = match self.loadw(memory, addr) {
                    Ok(w) => sign_ext_w!(w),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LD => {
                let val = match self.loadd(memory, addr) {
                    Ok(w) => w,
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LBU => {
                let val = match self.loadb(memory, addr) {
                    Ok(b) => zero_ext!(b),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LHU => {
                let val = match self.loadh(memory, addr) {
                    Ok(h) => zero_ext!(h),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            funct3::LWU if self.xlen() == BaseIsa::RV64I => {
                let val = match self.loadw(memory, addr) {
                    Ok(w) => zero_ext!(w),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_gpr(rd, val);
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_load_fp(&mut self, instr: Instruction, memory: &impl MemoryAccess) {
        if !self.ext_supported(Extension::F) {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();
        let rs = instr.rs1().value();
        let imm = self.expand_imm_i(instr.imm().value());

        // Need to account for the fact that RV32I is limited to 32-bit addr space
        let mut addr = self.read_gpr(rs).wrapping_add(imm);
        if matches!(self.xlen(), BaseIsa::RV32I) {
            addr &= 0xFFFF_FFFF;
        }

        match funct3 {
            funct3::FLW => {
                let val = match self.loadw(memory, addr) {
                    Ok(w) => sign_ext_w!(w),
                    Err(_) => {
                        self.trap(Trap::LoadAccessFault);
                        return;
                    }
                };
                self.write_fpr(rd, val, false);
            }
            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_custom_0(&mut self, instr: Instruction) {
        // This emulator defines custom0 instructions to use the I format
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();

        match funct3 {
            funct3::HALT => self.halted = true,
            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_misc_mem(&mut self, _instr: Instruction, _memory: &mut impl MemoryAccess) {
        /* Since this emulator naturally uses strong, sequential memory ordering, fences
         * are essentially noops. May revisit this in the future if emulating a weaker memory
         * model becomes a supported feature.
         */
        nop!();
    }

    pub(crate) fn handle_op_imm(&mut self, instr: Instruction) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();
        let rs = instr.rs1().value();
        let rs_val = self.read_gpr(rs);
        let imm = self.expand_imm_i(instr.imm().value());

        match funct3 {
            funct3::ADDI => {
                let res = rs_val.wrapping_add(imm);
                self.write_gpr(rd, res);
            }

            funct3::SLTI => {
                let cond = match self.xlen() {
                    BaseIsa::RV32I => (rs_val as i32) < (imm as i32),
                    BaseIsa::RV64I => (rs_val as i64) < (imm as i64),
                };
                let res = if cond { 1 } else { 0 };
                self.write_gpr(rd, res);
            }

            funct3::SLTIU => {
                let res = if rs_val < imm { 1 } else { 0 };
                self.write_gpr(rd, res);
            }

            funct3::ANDI => {
                let res = rs_val & imm;
                self.write_gpr(rd, res);
            }

            funct3::ORI => {
                let res = rs_val | imm;
                self.write_gpr(rd, res);
            }

            funct3::XORI => {
                let res = rs_val ^ imm;
                self.write_gpr(rd, res);
            }

            // Might be a shift operation
            _ => {
                let shopt = match self.xlen() {
                    BaseIsa::RV32I => instr.shopt().value(),

                    // This bit is used for shamt in RV64 so just mask it off to find shopt
                    BaseIsa::RV64I => instr.shopt().value() & !(1 << 3),
                };
                let shamt = match self.xlen() {
                    BaseIsa::RV32I => instr.shamt5().value(),
                    BaseIsa::RV64I => instr.shamt6().value(),
                };

                match shopt {
                    shopt::SLLI => {
                        let res = rs_val << shamt;
                        self.write_gpr(rd, res);
                    }

                    shopt::SRLI => {
                        let res = rs_val >> shamt;
                        self.write_gpr(rd, res);
                    }

                    shopt::SRAI => {
                        let res = match self.xlen() {
                            BaseIsa::RV32I => ((rs_val as i32) >> shamt) as u64,
                            BaseIsa::RV64I => ((rs_val as i64) >> shamt) as u64,
                        };
                        self.write_gpr(rd, res);
                    }

                    _ => self.trap(Trap::IllegalInstruction),
                }
            }
        }
    }

    pub(crate) fn handle_auipc(&mut self, instr: Instruction) {
        let instr = InstrFormatU::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let imm = self.expand_imm_u(instr.imm().value());
        let res = self.read_pc().wrapping_add(imm);
        self.write_gpr(rd, res);
    }

    pub(crate) fn handle_op_imm_32(&mut self, instr: Instruction) {
        if self.xlen() != BaseIsa::RV64I {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();
        let rs = instr.rs1().value();
        let rs_val = self.read_gpr(rs);
        let imm = self.expand_imm_i(instr.imm().value());

        match funct3 {
            funct3::ADDIW => {
                let res = sign_ext_w!(rs_val.wrapping_add(imm) as u32);
                self.write_gpr(rd, res);
            }

            _ => {
                let shopt = instr.shopt().value();
                let shamt = instr.shamt5().value();

                match shopt {
                    shopt::SLLIW => {
                        let res = sign_ext_w!((rs_val << shamt) as u32);
                        self.write_gpr(rd, res);
                    }

                    shopt::SRLIW => {
                        let res = sign_ext_w!((rs_val as u32) >> shamt);
                        self.write_gpr(rd, res);
                    }

                    shopt::SRAIW => {
                        let res = sign_ext_w!(((rs_val as i32) >> shamt) as u32);
                        self.write_gpr(rd, res);
                    }

                    _ => self.trap(Trap::IllegalInstruction),
                }
            }
        }
    }

    pub(crate) fn handle_b48(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_store(&mut self, instr: Instruction, memory: &mut impl MemoryAccess) {
        let instr = InstrFormatS::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rs1 = instr.rs1().value();
        let rs2 = instr.rs2().value();
        let rs2_val = self.read_gpr(rs2);
        let imm = self.expand_imm_s(instr.imm().value());

        // Need to account for the fact that RV32I is limited to 32-bit addr space
        let mut addr = self.read_gpr(rs1).wrapping_add(imm);
        if self.xlen() == BaseIsa::RV32I {
            addr &= 0xFFFF_FFFF;
        }

        match funct3 {
            funct3::SB => match self.storeb(memory, addr, rs2_val as u8) {
                Ok(()) => (),
                Err(_) => self.trap(Trap::StoreAccessFault),
            },

            funct3::SH => match self.storeh(memory, addr, rs2_val as u16) {
                Ok(()) => (),
                Err(_) => self.trap(Trap::StoreAccessFault),
            },

            funct3::SW => match self.storew(memory, addr, rs2_val as u32) {
                Ok(()) => (),
                Err(_) => self.trap(Trap::StoreAccessFault),
            },

            funct3::SD if self.xlen() == BaseIsa::RV64I => {
                match self.stored(memory, addr, rs2_val) {
                    Ok(()) => (),
                    Err(_) => self.trap(Trap::StoreAccessFault),
                }
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_store_fp(&mut self, instr: Instruction, memory: &mut impl MemoryAccess) {
        if !self.ext_supported(Extension::F) {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatS::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rs1 = instr.rs1().value();
        let rs2 = instr.rs2().value();
        let rs2_val = self.read_fpr(rs2, false);
        let imm = self.expand_imm_s(instr.imm().value());

        // Need to account for the fact that RV32I is limited to 32-bit addr space
        let mut addr = self.read_gpr(rs1).wrapping_add(imm);
        if self.xlen() == BaseIsa::RV32I {
            addr &= 0xFFFF_FFFF;
        }

        match funct3 {
            funct3::FSW => match self.storew(memory, addr, rs2_val as u32) {
                Ok(()) => (),
                Err(_) => self.trap(Trap::StoreAccessFault),
            },

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_custom_1(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_amo(&mut self, instr: Instruction, memory: &mut impl MemoryAccess) {
        if !self.ext_supported(Extension::A) {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatR::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let rs1 = instr.rs1().value();
        let addr = self.read_gpr(rs1);
        let rs2 = instr.rs2().value();
        let src = self.read_gpr(rs2);
        let amo = instr.amo().value();

        /* Because raven naturally uses a sequentially consistent load/store model,
         * the aq and rl bits in these instructions are ignored. If support for multiple
         * harts are added (and these are implemented via threads) may need to revisit and
         * consider these more.
         */
        match amo {
            amo::LRW => match self.loadw(memory, addr) {
                Ok(w) => {
                    self.write_gpr(rd, sign_ext_w!(w));
                    self.reserve_set = Some(ReserveSet::new(addr, w as u64));
                }
                Err(_) => self.trap(Trap::LoadAccessFault),
            },

            amo::SCW => match self.reserve_set {
                Some(rs) => {
                    // Load the current word in memory at addr
                    let word = match self.loadw(memory, addr) {
                        Ok(w) => w as u64,

                        // Shouldn't ever happen?
                        Err(_) => {
                            self.trap(Trap::LoadAccessFault);
                            return;
                        }
                    };

                    /* Now we need to ensure that the requested address is the same as in the
                     * reservation set (since the SC must be paired with the most recent LR)
                     * and that the word located there hasn't been modified since the LR call.
                     * If all okay, only then store the src value into memory.
                     */
                    if rs.addr == addr && rs.dword == word {
                        match self.storew(memory, addr, src as u32) {
                            Ok(()) => {
                                self.write_gpr(rd, 0);

                                // Invalidate this reservation set
                                self.reserve_set = None;
                            }
                            Err(_) => self.trap(Trap::StoreAccessFault),
                        }
                    }
                }

                // An LR has not been called for this SC
                None => {
                    self.write_gpr(rd, 1);
                }
            },

            amo::LRD => match self.loadd(memory, addr) {
                Ok(dw) => {
                    self.write_gpr(rd, dw);
                    self.reserve_set = Some(ReserveSet::new(addr, dw));
                }
                Err(_) => self.trap(Trap::LoadAccessFault),
            },

            amo::SCD => match self.reserve_set {
                Some(rs) => {
                    // Load the current double-word in memory at addr
                    let dword = match self.loadd(memory, addr) {
                        Ok(dw) => dw,

                        // Shouldn't ever happen?
                        Err(_) => {
                            self.trap(Trap::LoadAccessFault);
                            return;
                        }
                    };

                    /* Now we need to ensure that the requested address is the same as in the
                     * reservation set (since the SC must be paired with the most recent LR)
                     * and that the dword located there hasn't been modified since the LR call.
                     * If all okay, only then store the src value into memory.
                     */
                    if rs.addr == addr && rs.dword == dword {
                        match self.stored(memory, addr, src) {
                            Ok(()) => {
                                self.write_gpr(rd, 0);

                                // Invalidate this reservation set
                                self.reserve_set = None;
                            }
                            Err(_) => self.trap(Trap::StoreAccessFault),
                        }
                    }
                }

                // An LR has not been called for this SC
                None => {
                    self.write_gpr(rd, 1);
                }
            },

            amo::AMOSWAPW => self.amow(src, rd, addr, memory, |src, _| src),
            amo::AMOADDW => self.amow(src, rd, addr, memory, |src, w| src.wrapping_add(w)),
            amo::AMOANDW => self.amow(src, rd, addr, memory, |src, w| src & w),
            amo::AMOORW => self.amow(src, rd, addr, memory, |src, w| src | w),
            amo::AMOXORW => self.amow(src, rd, addr, memory, |src, w| src ^ w),
            amo::AMOMAXUW => self.amow(src, rd, addr, memory, std::cmp::max),
            amo::AMOMAXW => self.amow(src, rd, addr, memory, |src, w| {
                std::cmp::max(src as i32, w as i32) as u32
            }),
            amo::AMOMINUW => self.amow(src, rd, addr, memory, std::cmp::min),
            amo::AMOMINW => self.amow(src, rd, addr, memory, |src, w| {
                std::cmp::min(src as i32, w as i32) as u32
            }),

            amo::AMOSWAPD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, _| src)
            }
            amo::AMOADDD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| src.wrapping_add(w))
            }
            amo::AMOANDD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| src & w)
            }
            amo::AMOORD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| src | w)
            }
            amo::AMOXORD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| src ^ w)
            }
            amo::AMOMAXUD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, std::cmp::max)
            }
            amo::AMOMAXD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| {
                    std::cmp::max(src as i64, w as i64) as u64
                })
            }
            amo::AMOMINUD if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, std::cmp::min)
            }
            amo::AMOMIND if self.xlen() == BaseIsa::RV64I => {
                self.amod(src, rd, addr, memory, |src, w| {
                    std::cmp::min(src as i64, w as i64) as u64
                })
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_op(&mut self, instr: Instruction) {
        let instr = InstrFormatR::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let rs1 = instr.rs1().value();
        let rs1_val = self.read_gpr(rs1);
        let rs2 = instr.rs2().value();
        let rs2_val = self.read_gpr(rs2);
        let funct10 = instr.funct10().value();
        let shamt = match self.xlen() {
            BaseIsa::RV32I => rs2_val & 0b011111,
            BaseIsa::RV64I => rs2_val & 0b111111,
        };

        match funct10 {
            funct10::ADD => {
                let res = rs1_val.wrapping_add(rs2_val);
                self.write_gpr(rd, res);
            }

            funct10::SUB => {
                let res = rs1_val.wrapping_sub(rs2_val);
                self.write_gpr(rd, res);
            }

            funct10::SLT => {
                let cond = match self.xlen() {
                    BaseIsa::RV32I => (rs1_val as i32) < (rs2_val as i32),
                    BaseIsa::RV64I => (rs1_val as i64) < (rs2_val as i64),
                };
                let res = if cond { 1 } else { 0 };
                self.write_gpr(rd, res);
            }

            funct10::SLTU => {
                let res = if rs1_val < rs2_val { 1 } else { 0 };
                self.write_gpr(rd, res);
            }

            funct10::AND => {
                let res = rs1_val & rs2_val;
                self.write_gpr(rd, res);
            }

            funct10::OR => {
                let res = rs1_val | rs2_val;
                self.write_gpr(rd, res);
            }

            funct10::XOR => {
                let res = rs1_val ^ rs2_val;
                self.write_gpr(rd, res);
            }

            funct10::SLL => {
                let res = rs1_val << shamt;
                self.write_gpr(rd, res);
            }

            funct10::SRL => {
                let res = rs1_val >> shamt;
                self.write_gpr(rd, res);
            }

            funct10::SRA => {
                let res = match self.xlen() {
                    BaseIsa::RV32I => ((rs1_val as i32) >> shamt) as u64,
                    BaseIsa::RV64I => ((rs1_val as i64) >> shamt) as u64,
                };
                self.write_gpr(rd, res);
            }

            funct10::MUL if self.ext_supported(Extension::M) => {
                let res = rs1_val.wrapping_mul(rs2_val);
                self.write_gpr(rd, res);
            }

            funct10::MULH if self.ext_supported(Extension::M) => {
                let res = match self.xlen() {
                    BaseIsa::RV32I => {
                        let prod = ((rs1_val as i32) as i64).wrapping_mul((rs2_val as i32) as i64);
                        (prod >> 32) as u64
                    }
                    BaseIsa::RV64I => {
                        let prod =
                            ((rs1_val as i64) as i128).wrapping_mul((rs2_val as i64) as i128);
                        (prod >> 64) as u64
                    }
                };

                self.write_gpr(rd, res);
            }

            funct10::MULHSU if self.ext_supported(Extension::M) => {
                let res = match self.xlen() {
                    BaseIsa::RV32I => {
                        let prod = ((rs1_val as i32) as i64).wrapping_mul(rs2_val as i64);
                        (prod >> 32) as u64
                    }
                    BaseIsa::RV64I => {
                        let prod = ((rs1_val as i64) as i128).wrapping_mul(rs2_val as i128);
                        (prod >> 64) as u64
                    }
                };

                self.write_gpr(rd, res);
            }

            funct10::MULHU if self.ext_supported(Extension::M) => {
                let res = (rs1_val as u128).wrapping_mul(rs2_val as u128);
                let res = match self.xlen() {
                    BaseIsa::RV32I => (res >> 32) as u64,
                    BaseIsa::RV64I => (res >> 64) as u64,
                };
                self.write_gpr(rd, res);
            }

            funct10::DIV if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    match self.xlen() {
                        BaseIsa::RV32I => (rs1_val as i32).wrapping_div(rs2_val as i32) as u64,
                        BaseIsa::RV64I => (rs1_val as i64).wrapping_div(rs2_val as i64) as u64,
                    }
                } else {
                    // Divde by zero semantics state quotient should have all bits set
                    u64::MAX
                };

                self.write_gpr(rd, res);
            }

            funct10::DIVU if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    rs1_val / rs2_val
                } else {
                    // Divde by zero semantics state quotient should have all bits set
                    u64::MAX
                };

                self.write_gpr(rd, res);
            }

            funct10::REMU if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    rs1_val % rs2_val
                } else {
                    // Divide by zero semantics state remainder should equal dividend
                    rs1_val
                };

                self.write_gpr(rd, res);
            }

            funct10::REM if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    match self.xlen() {
                        BaseIsa::RV32I => ((rs1_val as i32).wrapping_rem(rs2_val as i32)) as u64,
                        BaseIsa::RV64I => ((rs1_val as i64).wrapping_rem(rs2_val as i64)) as u64,
                    }
                } else {
                    // Divide by zero semantics state remainder should equal dividend
                    rs1_val
                };

                self.write_gpr(rd, res);
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_lui(&mut self, instr: Instruction) {
        let instr = InstrFormatU::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let imm = self.expand_imm_u(instr.imm().value());

        self.write_gpr(rd, imm);
    }

    pub(crate) fn handle_op_32(&mut self, instr: Instruction) {
        if self.xlen() != BaseIsa::RV64I {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatR::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let rs1 = instr.rs1().value();
        let rs1_val = self.read_gpr(rs1);
        let rs2 = instr.rs2().value();
        let rs2_val = self.read_gpr(rs2);
        let funct10 = instr.funct10().value();
        let shamt = rs2_val & 0b011111;

        match funct10 {
            funct10::ADDW => {
                let res = sign_ext_w!(rs1_val.wrapping_add(rs2_val) as u32);
                self.write_gpr(rd, res);
            }

            funct10::SUBW => {
                let res = sign_ext_w!(rs1_val.wrapping_sub(rs2_val) as u32);
                self.write_gpr(rd, res);
            }

            funct10::SLLW => {
                let res = sign_ext_w!((rs1_val << shamt) as u32);
                self.write_gpr(rd, res);
            }

            funct10::SRLW => {
                let res = sign_ext_w!((rs1_val as u32) >> shamt);
                self.write_gpr(rd, res);
            }

            funct10::SRAW => {
                let res = sign_ext_w!((rs1_val as i32) >> shamt);
                self.write_gpr(rd, res);
            }

            funct10::MULW if self.ext_supported(Extension::M) => {
                let res = sign_ext_w!(rs1_val.wrapping_mul(rs2_val) as u32);
                self.write_gpr(rd, res);
            }

            funct10::DIVUW if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    sign_ext_w!((rs1_val as u32) / (rs2_val as u32))
                } else {
                    // Divde by zero semantics state quotient should have all bits set
                    u64::MAX
                };

                self.write_gpr(rd, res);
            }

            funct10::DIVW if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    sign_ext_w!((rs1_val as i32).wrapping_div(rs2_val as i32))
                } else {
                    // Divde by zero semantics state quotient should have all bits set
                    u64::MAX
                };

                self.write_gpr(rd, res);
            }

            funct10::REMUW if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    sign_ext_w!((rs1_val as u32) % (rs2_val as u32))
                } else {
                    // Divide by zero semantics state remainder should equal dividend
                    rs1_val
                };

                self.write_gpr(rd, res);
            }

            funct10::REMW if self.ext_supported(Extension::M) => {
                let res = if rs2_val != 0 {
                    sign_ext_w!((rs1_val as i32).wrapping_rem(rs2_val as i32))
                } else {
                    // Divide by zero semantics state remainder should equal dividend
                    rs1_val
                };

                self.write_gpr(rd, res);
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_b64(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_ffused(&mut self, instr: Instruction) {
        if !self.ext_supported(Extension::F) {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        let instr = InstrFormatR4::new_with_raw_value(instr.raw_value());
        let opcode = instr.opcode().major();
        let rd = instr.rd().value();
        let rs1 = instr.rs1().value();
        let rs2 = instr.rs2().value();
        let rs3 = instr.rs3().value();

        // Floating-point specific
        let fmt = instr.fmt();
        let frm = match instr.frm() {
            Frm::Dyn => self.reg.csr.fcsr.frm(),
            _ => instr.frm(),
        };
        let fprs1_val = self.read_fpr(rs1, false);
        let fprs2_val = self.read_fpr(rs2, false);
        let fprs3_val = self.read_fpr(rs3, false);
        let f1 = F32::from_bits(fprs1_val as u32);
        let f2 = F32::from_bits(fprs2_val as u32);
        let f3 = F32::from_bits(fprs3_val as u32);

        // Updated by softfloat floating point operations
        let mut fflags = ExceptionFlags::default();
        fflags.set();

        let res = match (opcode, fmt) {
            (MajorGroup::MAdd, FFmt::Single) => {
                f1.mul(f2, frm.into()).add(f3, frm.into()).to_bits()
            }
            (MajorGroup::NMAdd, FFmt::Single) => {
                f1.mul(f2, frm.into()).neg().sub(f3, frm.into()).to_bits()
            }
            (MajorGroup::MSub, FFmt::Single) => {
                f1.mul(f2, frm.into()).sub(f3, frm.into()).to_bits()
            }
            (MajorGroup::NMSub, FFmt::Single) => {
                f1.mul(f2, frm.into()).neg().add(f3, frm.into()).to_bits()
            }
            _ => {
                self.trap(Trap::IllegalInstruction);
                return;
            }
        };

        self.update_fflags(&mut fflags);
        self.write_fpr(rd, res as u64, false);
    }

    pub(crate) fn handle_op_fp(&mut self, instr: Instruction) {
        if !self.ext_supported(Extension::F) {
            self.trap(Trap::IllegalInstruction);
            return;
        }

        // General/integer regs
        let instr = InstrFormatR::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let rs1 = instr.rs1().value();
        let gprs1_val = self.read_gpr(rs1);
        let rs2 = instr.rs2().value();
        let funct7 = instr.funct7().value();
        let funct10 = instr.funct10().value();

        // Floating-point specific
        const SIGN: u64 = 1 << 31;
        let frm = match instr.frm() {
            Frm::Dyn => self.reg.csr.fcsr.frm(),
            _ => instr.frm(),
        };
        let fprs1_val = self.read_fpr(rs1, false);
        let fprs2_val = self.read_fpr(rs2, false);
        let f1 = F32::from_bits(fprs1_val as u32);
        let f2 = F32::from_bits(fprs2_val as u32);

        // Updated by softfloat floating point operations
        let mut fflags = ExceptionFlags::default();
        fflags.set();

        match funct7 {
            funct7::FADDS => {
                let res = f1.add(f2, frm.into()).to_bits();
                self.update_fflags(&mut fflags);
                self.write_fpr(rd, res as u64, false);
            }

            funct7::FSUBS => {
                let res = f1.sub(f2, frm.into()).to_bits();
                self.update_fflags(&mut fflags);
                self.write_fpr(rd, res as u64, false);
            }

            funct7::FMULS => {
                let res = f1.mul(f2, frm.into()).to_bits();
                self.update_fflags(&mut fflags);
                self.write_fpr(rd, res as u64, false);
            }

            funct7::FDIVS => {
                let res = f1.div(f2, frm.into()).to_bits();
                self.update_fflags(&mut fflags);
                self.write_fpr(rd, res as u64, false);
            }

            funct7::FSQRTS => {
                let res = f1.sqrt(frm.into()).to_bits();
                self.update_fflags(&mut fflags);
                self.write_fpr(rd, res as u64, false);
            }

            funct7::FMINMAXS => {
                /* RISCV considers -0.0 to be smaller than +0.0 for the purpose of this instruction,
                 * however the IEEE spec considers them equal, so can't rely completely on
                 * soft-float's lt compare method.
                 */
                let cond = match u8::from(frm.raw_value()) {
                    // Min
                    0b000 => (f1.is_negative_zero() && f2.is_positive_zero()) || f1.lt_quiet(f2),
                    // Max
                    0b001 => (f1.is_positive_zero() && f2.is_negative_zero()) || f2.lt_quiet(f1),
                    _ => {
                        self.trap(Trap::IllegalInstruction);
                        return;
                    }
                };
                self.update_fflags(&mut fflags);

                // Should return canonical NaN if both args are NaN
                let res = if f1.is_nan() && f2.is_nan() {
                    F32::quiet_nan()
                } else if cond {
                    f1
                } else {
                    f2
                }
                .to_bits();

                self.write_fpr(rd, res as u64, false);
            }

            // Peculiar instruction that uses rs2 field to encode behavior
            funct7::FCVTIS => match rs2 {
                // FCVT.W.S
                0b00000 => {
                    let res = sign_ext_w!(f1.to_i32(frm.into(), true));
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }

                // FCVT.WU.S
                0b00001 => {
                    let res = sign_ext_w!(f1.to_u32(frm.into(), true));
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }

                // FCVT.L.S
                0b00010 if self.xlen() == BaseIsa::RV64I => {
                    let res = f1.to_i64(frm.into(), true) as u64;
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }

                // FCVT.LU.S
                0b00011 if self.xlen() == BaseIsa::RV64I => {
                    let res = f1.to_u64(frm.into(), true);
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }

                _ => self.trap(Trap::IllegalInstruction),
            },

            funct7::FCVTSI => match rs2 {
                // FCVT.S.W
                0b00000 => {
                    let res = F32::from_i32(gprs1_val as i32, frm.into()).to_bits();
                    self.update_fflags(&mut fflags);
                    self.write_fpr(rd, res as u64, false);
                }

                // FCVT.S.WU
                0b00001 => {
                    let res = F32::from_u32(gprs1_val as u32, frm.into()).to_bits();
                    self.update_fflags(&mut fflags);
                    self.write_fpr(rd, res as u64, false);
                }

                // FCVT.S.L
                0b00010 if self.xlen() == BaseIsa::RV64I => {
                    let res = F32::from_i64(gprs1_val as i64, frm.into()).to_bits();
                    self.update_fflags(&mut fflags);
                    self.write_fpr(rd, res as u64, false);
                }

                // FCVT.S.LU
                0b00011 if self.xlen() == BaseIsa::RV64I => {
                    let res = F32::from_u64(gprs1_val, frm.into()).to_bits();
                    self.update_fflags(&mut fflags);
                    self.write_fpr(rd, res as u64, false);
                }

                _ => self.trap(Trap::IllegalInstruction),
            },

            _ => match funct10 {
                funct10::FMVXW => self.write_gpr(rd, sign_ext_w!(fprs1_val as u32)),
                funct10::FMVWX => self.write_fpr(rd, gprs1_val, false),

                funct10::FSGNJS => {
                    let res = (fprs1_val & !SIGN) | (fprs2_val & SIGN);
                    self.write_fpr(rd, res, false);
                }
                funct10::FSGNJNS => {
                    let res = (fprs1_val & !SIGN) | ((fprs2_val & SIGN) ^ SIGN);
                    self.write_fpr(rd, res, false);
                }
                funct10::FSGNJXS => {
                    let res = fprs1_val ^ (fprs2_val & SIGN);
                    self.write_fpr(rd, res, false);
                }

                funct10::FEQS => {
                    let res = if f1.eq(f2) { 1 } else { 0 };
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }
                funct10::FLTS => {
                    let res = if f1.lt(f2) { 1 } else { 0 };
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }
                funct10::FLES => {
                    let res = if f1.le(f2) { 1 } else { 0 };
                    self.update_fflags(&mut fflags);
                    self.write_gpr(rd, res);
                }

                funct10::FCLASSS => {
                    // See Table 29 in unprivileged spec for reference
                    let res = (f1.is_negative_infinity() as u16)
                        | ((f1.is_negative_normal() as u16) << 1)
                        | ((f1.is_negative_subnormal() as u16) << 2)
                        | ((f1.is_negative_zero() as u16) << 3)
                        | ((f1.is_positive_zero() as u16) << 4)
                        | ((f1.is_positive_subnormal() as u16) << 5)
                        | ((f1.is_positive_normal() as u16) << 6)
                        | ((f1.is_positive_infinity() as u16) << 7)
                        | ((f1.is_signaling_nan() as u16) << 8)
                        | (((f1.is_nan() && !f1.is_signaling_nan()) as u16) << 9);
                    self.write_gpr(rd, res as u64);
                }

                _ => self.trap(Trap::IllegalInstruction),
            },
        }
    }

    pub(crate) fn handle_op_v(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_custom_2(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_b48_2(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_branch(&mut self, instr: Instruction) {
        let instr = InstrFormatB::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rs1 = instr.rs1().value();
        let rs1_val = self.read_gpr(rs1);
        let rs2 = instr.rs2().value();
        let rs2_val = self.read_gpr(rs2);
        let addr = self.expand_imm_b(instr.imm().value());

        match funct3 {
            funct3::BEQ => {
                if rs1_val == rs2_val {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            funct3::BNE => {
                if rs1_val != rs2_val {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            funct3::BLT => {
                let cond = match self.xlen() {
                    BaseIsa::RV32I => (rs1_val as i32) < (rs2_val as i32),
                    BaseIsa::RV64I => (rs1_val as i64) < (rs2_val as i64),
                };
                if cond {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            funct3::BLTU => {
                if rs1_val < rs2_val {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            funct3::BGE => {
                let cond = match self.xlen() {
                    BaseIsa::RV32I => (rs1_val as i32) >= (rs2_val as i32),
                    BaseIsa::RV64I => (rs1_val as i64) >= (rs2_val as i64),
                };
                if cond {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            funct3::BGEU => {
                if rs1_val >= rs2_val {
                    if addr % 4 == 0 || self.ext_supported(Extension::C) {
                        self.write_pc_next_add(addr);
                    } else {
                        self.trap(Trap::InstructionAddressMisaligned);
                    }
                }
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_jalr(&mut self, instr: Instruction) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let rs = instr.rs1().value();
        let rd = instr.rd().value();
        let imm = self.expand_imm_i(instr.imm().value());

        // Must set the LSB of the resulting sum to 0
        let addr = self.read_gpr(rs).wrapping_add(imm) & !0b1;

        // Destination addr must be aligned to 4 bytes if C extension not active
        if addr % 4 != 0 && !self.ext_supported(Extension::C) {
            self.trap(Trap::InstructionAddressMisaligned);
            return;
        }

        // Store address of next instruction in rd
        self.write_gpr(rd, self.read_pc_next());

        // Then set next pc as result from above
        self.write_pc_next(addr);
    }

    pub(crate) fn handle_reserved(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_jal(&mut self, instr: Instruction) {
        let instr = InstrFormatJ::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let addr = self.expand_imm_j(instr.imm().value());

        // Destination addr must be aligned to 4 bytes if C extension not active
        if addr % 4 != 0 && !self.ext_supported(Extension::C) {
            self.trap(Trap::InstructionAddressMisaligned);
            return;
        }

        // Store address of next instruction in rd
        self.write_gpr(rd, self.read_pc_next());

        // Then set next pc as (current pc + imm)
        self.write_pc_next_add(addr);
    }

    pub(crate) fn handle_system(&mut self, instr: Instruction) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();

        /* rs1 represents source register for register Zicsr instructions,
         * but 5-bit unsigned immediate in Zicsr immediate variants.
         */
        let rs = instr.rs1().value();
        let rs_val = self.read_gpr(rs);
        let uimm = zero_ext!(instr.rs1().value());

        /* The imm field represents funct12 in PRIV instructions
         * but csr_addr in Zicsr instructions.
         */
        let funct12 = instr.imm().value();
        let csr_addr = instr.imm().value();

        match funct3 {
            funct3::PRIV => match funct12 {
                funct12::EBREAK => self.trap(Trap::Breakpoint),
                funct12::ECALL => match self.priv_mode {
                    PrivMode::User => self.trap(Trap::EnvironmentCallFromU),
                    PrivMode::Supervisor => self.trap(Trap::EnvironmentCallFromS),
                    PrivMode::Machine => self.trap(Trap::EnvironmentCallFromM),
                },

                // It is legal to implement WFI as a NOP as per spec.
                funct12::WFI => {
                    nop!();
                }

                funct12::MRET if self.priv_mode == PrivMode::Machine => {
                    // TODO: Handle interrupt stack pop
                    self.write_pc_next(self.reg.csr.mepc);
                }
                funct12::SRET if self.ext_supported(Extension::S) => todo!(),

                _ => self.trap(Trap::IllegalInstruction),
            },

            funct3::CSRRW if self.ext_supported(Extension::ZICSR) => {
                // The CSR should not be read at all if rd == x0
                let csr = if rd != 0 {
                    if let Ok(csr) = self.read_csr(csr_addr) {
                        csr
                    } else {
                        self.trap(Trap::IllegalInstruction);
                        return;
                    }
                } else {
                    0
                };

                if self.write_csr(csr_addr, rs_val).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            funct3::CSRRS if self.ext_supported(Extension::ZICSR) => {
                let csr = if let Ok(csr) = self.read_csr(csr_addr) {
                    csr
                } else {
                    self.trap(Trap::IllegalInstruction);
                    return;
                };

                // Write to CSR should not be performed at all if rs1 == x0
                if rs == 0 || self.write_csr(csr_addr, csr | rs_val).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            funct3::CSRRC if self.ext_supported(Extension::ZICSR) => {
                let csr = if let Ok(csr) = self.read_csr(csr_addr) {
                    csr
                } else {
                    self.trap(Trap::IllegalInstruction);
                    return;
                };

                // Write to CSR should not be performed at all if rs1 == x0
                if rs == 0 || self.write_csr(csr_addr, csr & !rs_val).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            funct3::CSRRWI if self.ext_supported(Extension::ZICSR) => {
                // The CSR should not be read at all if rd == x0
                let csr = if rd != 0 {
                    if let Ok(csr) = self.read_csr(csr_addr) {
                        csr
                    } else {
                        self.trap(Trap::IllegalInstruction);
                        return;
                    }
                } else {
                    0
                };

                if self.write_csr(csr_addr, uimm).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            funct3::CSRRSI if self.ext_supported(Extension::ZICSR) => {
                let csr = if let Ok(csr) = self.read_csr(csr_addr) {
                    csr
                } else {
                    self.trap(Trap::IllegalInstruction);
                    return;
                };

                // Write to CSR should not be performed at all if uimm == 0
                if uimm == 0 || self.write_csr(csr_addr, csr | uimm).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            funct3::CSRRCI if self.ext_supported(Extension::ZICSR) => {
                let csr = if let Ok(csr) = self.read_csr(csr_addr) {
                    csr
                } else {
                    self.trap(Trap::IllegalInstruction);
                    return;
                };

                // Write to CSR should not be performed at all if rs1 == x0
                if uimm == 0 || self.write_csr(csr_addr, csr & !uimm).is_ok() {
                    self.write_gpr(rd, csr);
                } else {
                    self.trap(Trap::IllegalInstruction);
                }
            }

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_op_ve(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_custom_3(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_b80(&mut self, _instr: Instruction) {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zero_ext() {
        assert_eq!(0x0000_0000_0000_0000u64, zero_ext!(0x00u8));
        assert_eq!(0x0000_0000_0000_00FFu64, zero_ext!(0xFFu8));
        assert_eq!(0x0000_0000_0000_00F0u64, zero_ext!(0xF0u8));
        assert_eq!(0x0000_0000_0000_0080u64, zero_ext!(0x80u8));
        assert_eq!(0x0000_0000_0000_0070u64, zero_ext!(0x70u8));
    }

    #[test]
    fn test_sign_ext_b() {
        assert_eq!(0x0000_0000_0000_0000u64, sign_ext_b!(0x00u8));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFFu64, sign_ext_b!(0xFFu8));
        assert_eq!(0xFFFF_FFFF_FFFF_FFF0u64, sign_ext_b!(0xF0u8));
        assert_eq!(0xFFFF_FFFF_FFFF_FF80u64, sign_ext_b!(0x80u8));
        assert_eq!(0x0000_0000_0000_0070u64, sign_ext_b!(0x70u8));
    }

    #[test]
    fn test_sign_ext_h() {
        assert_eq!(0x0000_0000_0000_0000u64, sign_ext_h!(0x0000u16));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFFu64, sign_ext_h!(0xFFFFu16));
        assert_eq!(0xFFFF_FFFF_FFFF_F000u64, sign_ext_h!(0xF000u16));
        assert_eq!(0xFFFF_FFFF_FFFF_8000u64, sign_ext_h!(0x8000u16));
        assert_eq!(0x0000_0000_0000_7000u64, sign_ext_h!(0x7000u16));
    }

    #[test]
    fn test_sign_ext_w() {
        assert_eq!(0x0000_0000_0000_0000u64, sign_ext_w!(0x0000_0000u32));
        assert_eq!(0xFFFF_FFFF_FFFF_0000u64, sign_ext_w!(0xFFFF_0000u32));
        assert_eq!(0xFFFF_FFFF_F000_0000u64, sign_ext_w!(0xF000_0000u32));
        assert_eq!(0xFFFF_FFFF_8000_0000u64, sign_ext_w!(0x8000_0000u32));
        assert_eq!(0x0000_0000_7000_0000u64, sign_ext_w!(0x7000_0000u32));
    }
}
