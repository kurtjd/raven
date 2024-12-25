use crate::cpu::{BaseIsa, Cpu};
use arbitrary_int::{u10, u12, u20, u3, u5, u6, u7};
use bitbybit::{bitenum, bitfield};
use softfloat_wrapper::RoundingMode;

pub(crate) mod funct3 {
    // RV32
    pub(crate) const BEQ: u8 = 0b000;
    pub(crate) const BNE: u8 = 0b001;
    pub(crate) const BLT: u8 = 0b100;
    pub(crate) const BGE: u8 = 0b101;
    pub(crate) const BLTU: u8 = 0b110;
    pub(crate) const BGEU: u8 = 0b111;
    pub(crate) const LB: u8 = 0b000;
    pub(crate) const LH: u8 = 0b001;
    pub(crate) const LW: u8 = 0b010;
    pub(crate) const LBU: u8 = 0b100;
    pub(crate) const LHU: u8 = 0b101;
    pub(crate) const SB: u8 = 0b000;
    pub(crate) const SH: u8 = 0b001;
    pub(crate) const SW: u8 = 0b010;
    pub(crate) const ADDI: u8 = 0b000;
    pub(crate) const SLTI: u8 = 0b010;
    pub(crate) const SLTIU: u8 = 0b011;
    pub(crate) const XORI: u8 = 0b100;
    pub(crate) const ORI: u8 = 0b110;
    pub(crate) const ANDI: u8 = 0b111;

    // RV64
    pub(crate) const LWU: u8 = 0b110;
    pub(crate) const LD: u8 = 0b011;
    pub(crate) const SD: u8 = 0b011;
    pub(crate) const ADDIW: u8 = 0b000;

    // Zicsr
    pub(crate) const PRIV: u8 = 0b000;
    pub(crate) const CSRRW: u8 = 0b001;
    pub(crate) const CSRRS: u8 = 0b010;
    pub(crate) const CSRRC: u8 = 0b011;
    pub(crate) const CSRRWI: u8 = 0b101;
    pub(crate) const CSRRSI: u8 = 0b110;
    pub(crate) const CSRRCI: u8 = 0b111;

    // F Extension
    pub(crate) const FLW: u8 = 0b010;
    pub(crate) const FSW: u8 = 0b010;
    pub(crate) const FLD: u8 = 0b011;
    pub(crate) const FSD: u8 = 0b011;

    // Custom
    /* Halts the CPU.
     * Full instruction: 0x0000_000B
     */
    pub(crate) const HALT: u8 = 0b000;
}

pub(crate) mod funct7 {
    // Floating-point
    pub(crate) const FADDS: u8 = 0b0000000;
    pub(crate) const FSUBS: u8 = 0b0000100;
    pub(crate) const FMULS: u8 = 0b0001000;
    pub(crate) const FDIVS: u8 = 0b0001100;
    pub(crate) const FSQRTS: u8 = 0b0101100;
    pub(crate) const FMINMAXS: u8 = 0b0010100;
    pub(crate) const FCVTIS: u8 = 0b1100000;
    pub(crate) const FCVTSI: u8 = 0b1101000;

    pub(crate) const FADDD: u8 = 0b0000001;
    pub(crate) const FSUBD: u8 = 0b0000101;
    pub(crate) const FMULD: u8 = 0b0001001;
    pub(crate) const FDIVD: u8 = 0b0001101;
    pub(crate) const FSQRTD: u8 = 0b0101101;
    pub(crate) const FMINMAXD: u8 = 0b0010101;
    pub(crate) const FCVTID: u8 = 0b1100001;
    pub(crate) const FCVTDI: u8 = 0b1101001;
    pub(crate) const FCVTSD: u8 = 0b0100000;
    pub(crate) const FCVTDS: u8 = 0b0100001;
}

#[allow(clippy::unusual_byte_groupings)]
pub(crate) mod funct10 {
    // RV32
    pub(crate) const ADD: u16 = 0b0000000_000;
    pub(crate) const SUB: u16 = 0b0100000_000;
    pub(crate) const SLL: u16 = 0b0000000_001;
    pub(crate) const SLT: u16 = 0b0000000_010;
    pub(crate) const SLTU: u16 = 0b0000000_011;
    pub(crate) const XOR: u16 = 0b0000000_100;
    pub(crate) const SRL: u16 = 0b0000000_101;
    pub(crate) const SRA: u16 = 0b0100000_101;
    pub(crate) const OR: u16 = 0b0000000_110;
    pub(crate) const AND: u16 = 0b0000000_111;

    // RV64
    pub(crate) const ADDW: u16 = 0b0000000_000;
    pub(crate) const SUBW: u16 = 0b0100000_000;
    pub(crate) const SLLW: u16 = 0b0000000_001;
    pub(crate) const SRLW: u16 = 0b0000000_101;
    pub(crate) const SRAW: u16 = 0b0100000_101;

    // M extension
    // RV32
    pub(crate) const MUL: u16 = 0b0000001_000;
    pub(crate) const MULH: u16 = 0b0000001_001;
    pub(crate) const MULHSU: u16 = 0b0000001_010;
    pub(crate) const MULHU: u16 = 0b0000001_011;
    pub(crate) const DIV: u16 = 0b0000001_100;
    pub(crate) const DIVU: u16 = 0b0000001_101;
    pub(crate) const REM: u16 = 0b0000001_110;
    pub(crate) const REMU: u16 = 0b0000001_111;

    // RV64
    pub(crate) const MULW: u16 = 0b0000001_000;
    pub(crate) const DIVW: u16 = 0b0000001_100;
    pub(crate) const DIVUW: u16 = 0b0000001_101;
    pub(crate) const REMW: u16 = 0b0000001_110;
    pub(crate) const REMUW: u16 = 0b0000001_111;

    // Floating-point
    pub(crate) const FMVXW: u16 = 0b1110000_000;
    pub(crate) const FMVWX: u16 = 0b1111000_000;
    pub(crate) const FSGNJS: u16 = 0b0010000_000;
    pub(crate) const FSGNJNS: u16 = 0b0010000_001;
    pub(crate) const FSGNJXS: u16 = 0b0010000_010;
    pub(crate) const FEQS: u16 = 0b1010000_010;
    pub(crate) const FLTS: u16 = 0b1010000_001;
    pub(crate) const FLES: u16 = 0b1010000_000;
    pub(crate) const FCLASSS: u16 = 0b1110000_001;

    pub(crate) const FMVXD: u16 = 0b1110001_000;
    pub(crate) const FMVDX: u16 = 0b1111001_000;
    pub(crate) const FSGNJD: u16 = 0b0010001_000;
    pub(crate) const FSGNJND: u16 = 0b0010001_001;
    pub(crate) const FSGNJXD: u16 = 0b0010001_010;
    pub(crate) const FEQD: u16 = 0b1010001_010;
    pub(crate) const FLTD: u16 = 0b1010001_001;
    pub(crate) const FLED: u16 = 0b1010001_000;
    pub(crate) const FCLASSD: u16 = 0b1110001_001;
}

#[allow(clippy::unusual_byte_groupings)]
pub(crate) mod shopt {
    // RV32
    pub(crate) const SLLI: u16 = 0b0000000_001;
    pub(crate) const SRLI: u16 = 0b0000000_101;
    pub(crate) const SRAI: u16 = 0b0100000_101;

    // RV64
    pub(crate) const SLLIW: u16 = 0b0000000_001;
    pub(crate) const SRLIW: u16 = 0b0000000_101;
    pub(crate) const SRAIW: u16 = 0b0100000_101;
}

// A extension
#[allow(clippy::unusual_byte_groupings)]
pub(crate) mod amo {
    // RV32
    pub(crate) const LRW: u8 = 0b00010_010;
    pub(crate) const SCW: u8 = 0b00011_010;
    pub(crate) const AMOSWAPW: u8 = 0b00001_010;
    pub(crate) const AMOADDW: u8 = 0b00000_010;
    pub(crate) const AMOXORW: u8 = 0b00100_010;
    pub(crate) const AMOANDW: u8 = 0b01100_010;
    pub(crate) const AMOORW: u8 = 0b01000_010;
    pub(crate) const AMOMINW: u8 = 0b10000_010;
    pub(crate) const AMOMAXW: u8 = 0b10100_010;
    pub(crate) const AMOMINUW: u8 = 0b11000_010;
    pub(crate) const AMOMAXUW: u8 = 0b11100_010;

    // RV64
    pub(crate) const LRD: u8 = 0b00010_011;
    pub(crate) const SCD: u8 = 0b00011_011;
    pub(crate) const AMOSWAPD: u8 = 0b00001_011;
    pub(crate) const AMOADDD: u8 = 0b00000_011;
    pub(crate) const AMOXORD: u8 = 0b00100_011;
    pub(crate) const AMOANDD: u8 = 0b01100_011;
    pub(crate) const AMOORD: u8 = 0b01000_011;
    pub(crate) const AMOMIND: u8 = 0b10000_011;
    pub(crate) const AMOMAXD: u8 = 0b10100_011;
    pub(crate) const AMOMINUD: u8 = 0b11000_011;
    pub(crate) const AMOMAXUD: u8 = 0b11100_011;
}

pub(crate) mod funct12 {
    // RV32
    pub(crate) const ECALL: u16 = 0b000000000000;
    pub(crate) const EBREAK: u16 = 0b000000000001;
    pub(crate) const WFI: u16 = 0b000100000101;
    pub(crate) const MRET: u16 = 0b001100000010;
    pub(crate) const SRET: u16 = 0b000100000010;
}

#[bitenum(u5, exhaustive = true)]
pub(crate) enum MajorGroup {
    Load = 0b00_000,
    LoadFP = 0b00_001,
    Custom0 = 0b00_010,
    MiscMem = 0b00_011,
    OpImm = 0b00_100,
    AuiPC = 0b00_101,
    OpImm32 = 0b00_110,
    B48 = 0b00_111,

    Store = 0b01_000,
    StoreFP = 0b01_001,
    Custom1 = 0b01_010,
    AMO = 0b01_011,
    Op = 0b01_100,
    Lui = 0b01_101,
    Op32 = 0b01_110,
    B64 = 0b01_111,

    MAdd = 0b10_000,
    MSub = 0b10_001,
    NMSub = 0b10_010,
    NMAdd = 0b10_011,
    OpFP = 0b10_100,
    OpV = 0b10_101,
    Custom2 = 0b10_110,
    B48_2 = 0b10_111,

    Branch = 0b11_000,
    Jalr = 0b11_001,
    Reserved = 0b11_010,
    Jal = 0b11_011,
    System = 0b11_100,
    OpVE = 0b11_101,
    Custom3 = 0b11_110,
    B80 = 0b11_111,
}

#[bitenum(u2, exhaustive = true)]
pub(crate) enum ILenGroup {
    B16Q0 = 0b00,
    B16Q1 = 0b01,
    B16Q2 = 0b10,
    B32 = 0b11,
}

#[bitfield(u7)]
pub(crate) struct Opcode {
    #[bits(0..=1, r)]
    ilen: ILenGroup,

    #[bits(2..=6, r)]
    major: MajorGroup,
}

#[bitfield(u32)]
pub(crate) struct Instruction {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(0..=15, r)]
    half: u16,
}

#[bitenum(u3, exhaustive = true)]
pub(crate) enum Frm {
    Rne = 0b000,
    Rtz = 0b001,
    Rdn = 0b010,
    Rup = 0b011,
    Rmm = 0b100,
    Res1 = 0b101,
    Res2 = 0b110,
    Dyn = 0b111,
}

impl From<Frm> for RoundingMode {
    fn from(frm: Frm) -> Self {
        match frm {
            Frm::Rtz => Self::TowardZero,
            Frm::Rdn => Self::TowardNegative,
            Frm::Rup => Self::TowardPositive,
            Frm::Rmm => Self::TiesToAway,
            _ => Self::TiesToEven,
        }
    }
}

#[bitfield(u32)]
pub(crate) struct InstrFormatR {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(7..=11, r)]
    rd: u5,

    #[bits(12..=14, r)]
    funct3: u3,

    #[bits(12..=14, r)]
    frm: Frm,

    #[bits(15..=19, r)]
    rs1: u5,

    #[bits(20..=24, r)]
    rs2: u5,

    #[bits(25..=31, r)]
    funct7: u7,

    // Not an actual field, but useful for creating single unique funct ID
    #[bits([12..=14, 25..=31], r)]
    funct10: u10,

    // For identifying atomics
    #[bits([12..=14, 27..=31], r)]
    amo: u8,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatI {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(7..=11, r)]
    rd: u5,

    #[bits(12..=14, r)]
    funct3: u3,

    #[bits(15..=19, r)]
    rs1: u5,

    #[bits(20..=31, r)]
    imm: u12,

    #[bits(20..=24, r)]
    shamt5: u5,

    #[bits(20..=25, r)]
    shamt6: u6,

    // Not actual fields, but useful for creating single unique shift op ID
    #[bits([12..=14, 25..=31], r)]
    shopt: u10,

    // An alias for the imm field, used for SYSTEM opcodes
    #[bits(20..=31, r)]
    funct12: u12,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatS {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(12..=14, r)]
    funct3: u3,

    #[bits(15..=19, r)]
    rs1: u5,

    #[bits(20..=24, r)]
    rs2: u5,

    #[bits([7..=11, 25..=31], r)]
    imm: u12,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatB {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(12..=14, r)]
    funct3: u3,

    #[bits(15..=19, r)]
    rs1: u5,

    #[bits(20..=24, r)]
    rs2: u5,

    #[bits([8..=11, 25..=30, 7, 31], r)]
    imm: u12,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatU {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(7..=11, r)]
    rd: u5,

    #[bits(12..=31, r)]
    imm: u20,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatJ {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(7..=11, r)]
    rd: u5,

    #[bits([21..=30, 20, 12..=19, 31], r)]
    imm: u20,
}

#[bitenum(u2, exhaustive = true)]
pub(crate) enum FFmt {
    Single = 0b00,
    Double = 0b01,
    Half = 0b10,
    Quad = 0b11,
}

#[bitfield(u32)]
pub(crate) struct InstrFormatR4 {
    #[bits(0..=6, r)]
    opcode: Opcode,

    #[bits(7..=11, r)]
    rd: u5,

    #[bits(12..=14, r)]
    frm: Frm,

    #[bits(15..=19, r)]
    rs1: u5,

    #[bits(20..=24, r)]
    rs2: u5,

    #[bits(25..=26, r)]
    fmt: FFmt,

    #[bits(27..=31, r)]
    rs3: u5,
}

impl Cpu {
    pub(crate) fn expand_imm_i(&self, imm: u16) -> u64 {
        let imm = (((imm << 4) as i16) >> 4) as u64;

        match self.xlen() {
            BaseIsa::RV32I => imm & 0xFFFF_FFFF,
            BaseIsa::RV64I => imm,
        }
    }

    pub(crate) fn expand_imm_s(&self, imm: u16) -> u64 {
        self.expand_imm_i(imm)
    }

    pub(crate) fn expand_imm_b(&self, imm: u16) -> u64 {
        let imm = (((imm << 4) as i16) >> 3) as u64;

        match self.xlen() {
            BaseIsa::RV32I => imm & 0xFFFF_FFFF,
            BaseIsa::RV64I => imm,
        }
    }

    pub(crate) fn expand_imm_u(&self, imm: u32) -> u64 {
        let imm = ((imm << 12) as i32) as u64;

        match self.xlen() {
            BaseIsa::RV32I => imm & 0xFFFF_FFFF,
            BaseIsa::RV64I => imm,
        }
    }

    pub(crate) fn expand_imm_j(&self, imm: u32) -> u64 {
        let imm = (((imm << 12) as i32) >> 11) as u64;

        match self.xlen() {
            BaseIsa::RV32I => imm & 0xFFFF_FFFF,
            BaseIsa::RV64I => imm,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_imm_i() {
        let cpu32 = Cpu::new("RV32I", 0).unwrap();
        let cpu64 = Cpu::new("RV64I", 0).unwrap();

        assert_eq!(0x0000_0000_0000_0000u64, cpu32.expand_imm_i(0x000u16));
        assert_eq!(0x0000_0000_0000_0000u64, cpu64.expand_imm_i(0x000u16));

        assert_eq!(0x0000_0000_FFFF_FFFFu64, cpu32.expand_imm_i(0xFFFu16));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFFu64, cpu64.expand_imm_i(0xFFFu16));

        assert_eq!(0x0000_0000_FFFF_FF00u64, cpu32.expand_imm_i(0xF00u16));
        assert_eq!(0xFFFF_FFFF_FFFF_FF00u64, cpu64.expand_imm_i(0xF00u16));

        assert_eq!(0x0000_0000_FFFF_F800u64, cpu32.expand_imm_i(0x800u16));
        assert_eq!(0xFFFF_FFFF_FFFF_F800u64, cpu64.expand_imm_i(0x800u16));

        assert_eq!(0x0000_0000_0000_0700u64, cpu32.expand_imm_i(0x700u16));
        assert_eq!(0x0000_0000_0000_0700u64, cpu64.expand_imm_i(0x700u16));
    }

    #[test]
    fn test_expand_imm_s() {
        let cpu32 = Cpu::new("RV32I", 0).unwrap();
        let cpu64 = Cpu::new("RV64I", 0).unwrap();

        assert_eq!(0x0000_0000_0000_0000u64, cpu32.expand_imm_s(0x000u16));
        assert_eq!(0x0000_0000_0000_0000u64, cpu64.expand_imm_s(0x000u16));

        assert_eq!(0x0000_0000_FFFF_FFFFu64, cpu32.expand_imm_s(0xFFFu16));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFFu64, cpu64.expand_imm_s(0xFFFu16));

        assert_eq!(0x0000_0000_FFFF_FF00u64, cpu32.expand_imm_s(0xF00u16));
        assert_eq!(0xFFFF_FFFF_FFFF_FF00u64, cpu64.expand_imm_s(0xF00u16));

        assert_eq!(0x0000_0000_FFFF_F800u64, cpu32.expand_imm_s(0x800u16));
        assert_eq!(0xFFFF_FFFF_FFFF_F800u64, cpu64.expand_imm_s(0x800u16));

        assert_eq!(0x0000_0000_0000_0700u64, cpu32.expand_imm_s(0x700u16));
        assert_eq!(0x0000_0000_0000_0700u64, cpu64.expand_imm_s(0x700u16));
    }

    #[test]
    fn test_expand_imm_b() {
        let cpu32 = Cpu::new("RV32I", 0).unwrap();
        let cpu64 = Cpu::new("RV64I", 0).unwrap();

        assert_eq!(0x0000_0000_0000_0000u64, cpu32.expand_imm_b(0x000u16));
        assert_eq!(0x0000_0000_0000_0000u64, cpu64.expand_imm_b(0x000u16));

        assert_eq!(0x0000_0000_FFFF_FFFEu64, cpu32.expand_imm_b(0xFFFu16));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFEu64, cpu64.expand_imm_b(0xFFFu16));

        assert_eq!(0x0000_0000_FFFF_FE02u64, cpu32.expand_imm_b(0xF01u16));
        assert_eq!(0xFFFF_FFFF_FFFF_FE02u64, cpu64.expand_imm_b(0xF01u16));

        assert_eq!(0x0000_0000_FFFF_F002u64, cpu32.expand_imm_b(0x801u16));
        assert_eq!(0xFFFF_FFFF_FFFF_F002u64, cpu64.expand_imm_b(0x801u16));

        assert_eq!(0x0000_0000_0000_0E00u64, cpu32.expand_imm_b(0x700u16));
        assert_eq!(0x0000_0000_0000_0E00u64, cpu64.expand_imm_b(0x700u16));
    }

    #[test]
    fn test_expand_imm_u() {
        let cpu32 = Cpu::new("RV32I", 0).unwrap();
        let cpu64 = Cpu::new("RV64I", 0).unwrap();

        assert_eq!(0x0000_0000_0000_0000u64, cpu32.expand_imm_u(0x00000u32));
        assert_eq!(0x0000_0000_0000_0000u64, cpu64.expand_imm_u(0x00000u32));

        assert_eq!(0x0000_0000_FFFF_F000u64, cpu32.expand_imm_u(0xFFFFFu32));
        assert_eq!(0xFFFF_FFFF_FFFF_F000u64, cpu64.expand_imm_u(0xFFFFFu32));

        assert_eq!(0x0000_0000_F000_0000u64, cpu32.expand_imm_u(0xF0000u32));
        assert_eq!(0xFFFF_FFFF_F000_0000u64, cpu64.expand_imm_u(0xF0000u32));

        assert_eq!(0x0000_0000_8000_0000u64, cpu32.expand_imm_u(0x80000u32));
        assert_eq!(0xFFFF_FFFF_8000_0000u64, cpu64.expand_imm_u(0x80000u32));

        assert_eq!(0x0000_0000_7000_0000u64, cpu32.expand_imm_u(0x70000u32));
        assert_eq!(0x0000_0000_7000_0000u64, cpu64.expand_imm_u(0x70000u32));
    }

    #[test]
    fn test_expand_imm_j() {
        let cpu32 = Cpu::new("RV32I", 0).unwrap();
        let cpu64 = Cpu::new("RV64I", 0).unwrap();

        assert_eq!(0x0000_0000_0000_0000u64, cpu32.expand_imm_j(0x00000u32));
        assert_eq!(0x0000_0000_0000_0000u64, cpu64.expand_imm_j(0x00000u32));

        assert_eq!(0x0000_0000_FFFF_FFFEu64, cpu32.expand_imm_j(0xFFFFFu32));
        assert_eq!(0xFFFF_FFFF_FFFF_FFFEu64, cpu64.expand_imm_j(0xFFFFFu32));

        assert_eq!(0x0000_0000_FFFE_0002u64, cpu32.expand_imm_j(0xF0001u32));
        assert_eq!(0xFFFF_FFFF_FFFE_0002u64, cpu64.expand_imm_j(0xF0001u32));

        assert_eq!(0x0000_0000_FFF0_0002u64, cpu32.expand_imm_j(0x80001u32));
        assert_eq!(0xFFFF_FFFF_FFF0_0002u64, cpu64.expand_imm_j(0x80001u32));

        assert_eq!(0x0000_0000_000E_0000u64, cpu32.expand_imm_j(0x70000u32));
        assert_eq!(0x0000_0000_000E_0000u64, cpu64.expand_imm_j(0x70000u32));
    }
}
