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

    pub(crate) fn handle_load_fp(&mut self, _instr: Instruction, _memory: &impl MemoryAccess) {
        todo!();
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

    pub(crate) fn handle_store_fp(&mut self, _instr: Instruction, _memory: &mut impl MemoryAccess) {
        todo!();
    }

    pub(crate) fn handle_custom_1(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_amo(&mut self, _instr: Instruction, _memory: &mut impl MemoryAccess) {
        todo!();
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

            _ => self.trap(Trap::IllegalInstruction),
        }
    }

    pub(crate) fn handle_b64(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_madd(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_msub(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_nmsub(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_nmadd(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_op_fp(&mut self, _instr: Instruction) {
        todo!();
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
