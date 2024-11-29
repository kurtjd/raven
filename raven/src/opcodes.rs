use crate::cpu::*;
use crate::instr_formats::*;
use crate::memory::*;

// Sign extends a byte to a double-word
macro_rules! sign_ext_b {
    ($val:expr) => {
        (($val as i8) as u64)
    };
}

// Sign extends a half-word to a double-word
macro_rules! sign_ext_h {
    ($val:expr) => {
        (($val as i16) as u64)
    };
}

// Sign extends a word to a double-word
macro_rules! sign_ext_w {
    ($val:expr) => {
        (($val as i32) as u64)
    };
}

// Zero extends any value to a double-word
macro_rules! zero_ext {
    ($val:expr) => {
        ($val as u64)
    };
}

/* Not needed for functionality, but helpful for explicitly
 * showing an opcode should behave as a noop.
 */
macro_rules! noop {
    () => {};
}

impl Cpu {
    pub(crate) fn handle_load(&mut self, instr: Instruction, memory: &impl MemoryAccess) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let funct3 = instr.funct3().value();
        let rd = instr.rd().value();
        let rs = instr.rs1().value();
        let imm = self.expand_imm_i(instr.imm().value());
        let addr = self.read_gpr(rs).wrapping_add(imm);

        match funct3 {
            funct3::LB => {
                let val = match memory.loadb(addr) {
                    Ok(b) => sign_ext_b!(b),
                    Err(_) => todo!(),
                };
                self.write_gpr(rd, val);
            }

            funct3::LH => {
                let val = match memory.loadh(addr) {
                    Ok(h) => sign_ext_h!(h),
                    Err(_) => todo!(),
                };
                self.write_gpr(rd, val);
            }

            funct3::LW => {
                let val = match memory.loadw(addr) {
                    Ok(w) => sign_ext_w!(w),
                    Err(_) => todo!(),
                };
                self.write_gpr(rd, val);
            }

            funct3::LBU => {
                let val = match memory.loadb(addr) {
                    Ok(b) => zero_ext!(b),
                    Err(_) => todo!(),
                };
                self.write_gpr(rd, val);
            }

            funct3::LHU => {
                let val = match memory.loadh(addr) {
                    Ok(h) => zero_ext!(h),
                    Err(_) => todo!(),
                };
                self.write_gpr(rd, val);
            }

            // Handle invalid instruction exception
            _ => todo!(),
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

            // Handle invalid instruction exception
            _ => todo!(),
        }
    }

    pub(crate) fn handle_misc_mem(&mut self, _instr: Instruction, _memory: &mut impl MemoryAccess) {
        /* Since this emulator naturally uses strong, sequential memory ordering, fences
         * are essentially noops. May revisit this in the future if emulating a weaker memory
         * model becomes a supported feature.
         */
        noop!();
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
                let cond = match self.base_isa {
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

            _ => {
                /* We treat shift ops as a R-type format, even though technically they are just a
                 * special case of the I-type format. This is because we can use funct7 with funct3
                 * (as a funct10) to identify SRAI from SRLI and we can use rs2 as the shift amount.
                 */
                let instr = InstrFormatR::new_with_raw_value(instr.raw_value());
                let funct10 = instr.funct10().value();
                let shamt = instr.rs2().value();

                match funct10 {
                    funct10::SLLI => {
                        let res = rs_val << shamt;
                        self.write_gpr(rd, res);
                    }

                    funct10::SRLI => {
                        let res = rs_val >> shamt;
                        self.write_gpr(rd, res);
                    }

                    funct10::SRAI => {
                        let res = match self.base_isa {
                            BaseIsa::RV32I => ((rs_val as i32) >> shamt) as u64,
                            BaseIsa::RV64I => ((rs_val as i64) >> shamt) as u64,
                        };
                        self.write_gpr(rd, res);
                    }

                    // Handle invalid instruction exception
                    _ => todo!(),
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

    pub(crate) fn handle_op_imm_32(&mut self, _instr: Instruction) {
        todo!();
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
        let addr = self.read_gpr(rs1).wrapping_add(imm);

        match funct3 {
            funct3::SB => match memory.storeb(addr, rs2_val as u8) {
                Ok(()) => (),
                Err(_) => todo!(),
            },

            funct3::SH => match memory.storeh(addr, rs2_val as u16) {
                Ok(()) => (),
                Err(_) => todo!(),
            },

            funct3::SW => match memory.storew(addr, rs2_val as u32) {
                Ok(()) => (),
                Err(_) => todo!(),
            },

            // Handle invalid instruction exception
            _ => todo!(),
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

        // Shift operations only use the 5 LSBs of rs2 for the shift amount
        let shamt = rs2_val & 0b11111;

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
                let cond = match self.base_isa {
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
                let res = match self.base_isa {
                    BaseIsa::RV32I => ((rs1_val as i32) >> shamt) as u64,
                    BaseIsa::RV64I => ((rs1_val as i64) >> shamt) as u64,
                };
                self.write_gpr(rd, res);
            }

            // Handle invalid instruction exception
            _ => todo!(),
        }
    }

    pub(crate) fn handle_lui(&mut self, instr: Instruction) {
        let instr = InstrFormatU::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let imm = self.expand_imm_u(instr.imm().value());

        self.write_gpr(rd, imm);
    }

    pub(crate) fn handle_op_32(&mut self, _instr: Instruction) {
        todo!();
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
        let imm = self.expand_imm_b(instr.imm().value());

        match funct3 {
            funct3::BEQ => {
                if rs1_val == rs2_val {
                    self.write_pc_next_add(imm);
                }
            }

            funct3::BNE => {
                if rs1_val != rs2_val {
                    self.write_pc_next_add(imm);
                }
            }

            funct3::BLT => {
                let cond = match self.base_isa {
                    BaseIsa::RV32I => (rs1_val as i32) < (rs2_val as i32),
                    BaseIsa::RV64I => (rs1_val as i64) < (rs2_val as i64),
                };
                if cond {
                    self.write_pc_next_add(imm);
                }
            }

            funct3::BLTU => {
                if rs1_val < rs2_val {
                    self.write_pc_next_add(imm);
                }
            }

            funct3::BGE => {
                let cond = match self.base_isa {
                    BaseIsa::RV32I => (rs1_val as i32) >= (rs2_val as i32),
                    BaseIsa::RV64I => (rs1_val as i64) >= (rs2_val as i64),
                };
                if cond {
                    self.write_pc_next_add(imm);
                }
            }

            funct3::BGEU => {
                if rs1_val >= rs2_val {
                    self.write_pc_next_add(imm);
                }
            }

            // Handle invalid instruction exception
            _ => todo!(),
        }
    }

    pub(crate) fn handle_jalr(&mut self, instr: Instruction) {
        let instr = InstrFormatI::new_with_raw_value(instr.raw_value());
        let rs = instr.rs1().value();
        let rd = instr.rd().value();
        let imm = self.expand_imm_i(instr.imm().value());

        // Must set the LSB of the resulting sum to 0
        let res = self.read_gpr(rs).wrapping_add(imm) & !0b1;

        // Store address of next instruction in rd
        self.write_gpr(rd, self.read_pc_next());

        // Then set next pc as result from above
        self.write_pc_next(res);
    }

    pub(crate) fn handle_reserved(&mut self, _instr: Instruction) {
        todo!();
    }

    pub(crate) fn handle_jal(&mut self, instr: Instruction) {
        let instr = InstrFormatJ::new_with_raw_value(instr.raw_value());
        let rd = instr.rd().value();
        let imm = self.expand_imm_j(instr.imm().value());

        // Store address of next instruction in rd
        self.write_gpr(rd, self.read_pc_next());

        // Then set next pc as (current pc + imm)
        self.write_pc_next_add(imm);
    }

    pub(crate) fn handle_system(&mut self, _instr: Instruction) {
        todo!();
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
