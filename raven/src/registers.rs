use crate::cpu::*;
use crate::csr::Csr;

#[derive(Default)]
pub(crate) struct Registers {
    pub(crate) pc: u64,
    pub(crate) pc_next: u64,
    pub(crate) gpr: [u64; 32],
    pub(crate) _fpr: [u64; 32],
    pub(crate) csr: Csr,
}

impl Cpu {
    pub(crate) fn read_pc(&self) -> u64 {
        match self.xlen() {
            BaseIsa::RV32I => self.reg.pc & 0xFFFF_FFFF,
            BaseIsa::RV64I => self.reg.pc,
        }
    }

    pub(crate) fn read_pc_next(&self) -> u64 {
        match self.xlen() {
            BaseIsa::RV32I => self.reg.pc_next & 0xFFFF_FFFF,
            BaseIsa::RV64I => self.reg.pc_next,
        }
    }

    pub(crate) fn write_pc_next(&mut self, pc: u64) {
        match self.xlen() {
            BaseIsa::RV32I => self.reg.pc_next = sign_ext_w!(pc as u32),
            BaseIsa::RV64I => self.reg.pc_next = pc,
        }
    }

    pub(crate) fn write_pc_next_add(&mut self, val: u64) {
        let pc = self.reg.pc.wrapping_add(val);

        match self.xlen() {
            BaseIsa::RV32I => self.reg.pc_next = sign_ext_w!(pc as u32),
            BaseIsa::RV64I => self.reg.pc_next = pc,
        }
    }

    pub(crate) fn update_pc(&mut self) {
        self.reg.pc = self.reg.pc_next;
    }

    pub(crate) fn read_gpr(&self, rs: u8) -> u64 {
        match self.xlen() {
            BaseIsa::RV32I => self.reg.gpr[rs as usize] & 0xFFFF_FFFF,
            BaseIsa::RV64I => self.reg.gpr[rs as usize],
        }
    }

    pub(crate) fn write_gpr(&mut self, rd: u8, val: u64) {
        if rd != 0 {
            match self.xlen() {
                BaseIsa::RV32I => self.reg.gpr[rd as usize] = sign_ext_w!(val as u32),
                BaseIsa::RV64I => self.reg.gpr[rd as usize] = val,
            }
        }
    }
}
