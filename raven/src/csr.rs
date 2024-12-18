use crate::cpu::*;
use arbitrary_int::*;
use bitbybit::{bitenum, bitfield};

// Denotes non-commerical implementation
const RAVEN_VENDOR_ID: u32 = 0;

// As this is an emulator with no recognized microarchitecture, denotes not implemented
const RAVEN_ARCH_ID: u64 = 0;

// Denotes the current version of raven
const RAVEN_IMPL_ID: u64 = 1;

#[non_exhaustive]
pub(crate) enum CsrError {
    InsufficientPrivilege,
    NotSupported,
    WriteToReadOnly,
}

mod csr_addr {
    // Machine Information Registers
    pub(super) const MVENDORID: u16 = 0xF11;
    pub(super) const MARCHID: u16 = 0xF12;
    pub(super) const MIMPID: u16 = 0xF13;
    pub(super) const MHARTID: u16 = 0xF14;
    pub(super) const MCONFIGPTR: u16 = 0xF15;

    // Machine Trap Setup
    pub(super) const MSTATUS: u16 = 0x300;
    pub(super) const MISA: u16 = 0x301;
    pub(super) const MEDELEG: u16 = 0x302;
    pub(super) const MIDELEG: u16 = 0x303;
    pub(super) const MIE: u16 = 0x304;
    pub(super) const MTVEC: u16 = 0x305;
    pub(super) const MCOUNTEREN: u16 = 0x306;
    pub(super) const MSTATUSH: u16 = 0x310;
    pub(super) const MEDELEGH: u16 = 0x312;

    // Machine Trap Handling
    pub(super) const MSCRATCH: u16 = 0x340;
    pub(super) const MEPC: u16 = 0x341;
    pub(super) const MCAUSE: u16 = 0x342;
    pub(super) const MTVAL: u16 = 0x343;
    pub(super) const MIP: u16 = 0x344;

    // Machine configuration
    pub(super) const MENVCFG: u16 = 0x30A;
    pub(super) const MENVCFGH: u16 = 0x31A;
    pub(super) const MSECCFG: u16 = 0x747;
    pub(super) const MSECCFGH: u16 = 0x757;

    // Machine Counters/Timers
    pub(super) const MCYCLE: u16 = 0xB00;
    pub(super) const MINSTRET: u16 = 0xB02;
    pub(super) const MCYCLEH: u16 = 0xB80;
    pub(super) const MINSTRETH: u16 = 0xB82;

    // Machine Counter Setup
    pub(super) const MCOUNTINHIBIT: u16 = 0x320;

    /* These are not implemented by Raven currently and any
     * addr in these ranges maps to read-only zero.
     */
    pub(super) const MHPMCOUNTER_START: u16 = 0xB03;
    pub(super) const MHPMCOUNTER_END: u16 = 0xB1F;
    pub(super) const MHPMCOUNTERH_START: u16 = 0xB83;
    pub(super) const MHPMCOUNTERH_END: u16 = 0xB9F;
}

#[bitenum(u2, exhaustive = true)]
pub(crate) enum PrivLevel {
    User = 0b00,
    Supervisor = 0b01,
    Reserved = 0b10,
    Machine = 0b11,
}

#[derive(PartialEq)]
#[bitenum(u2, exhaustive = true)]
enum Rw {
    Rw0 = 0b00,
    Rw1 = 0b01,
    Rw2 = 0b10,
    Ro = 0b11,
}

#[bitfield(u16)]
struct CsrAddr {
    #[bits(8..=9, r)]
    priv_lvl: PrivLevel,

    #[bits(10..=11, r)]
    rw: Rw,

    #[bits(12..=15, r)]
    illegal: u4,
}

#[bitenum(u2, exhaustive = true)]
pub(crate) enum Mxl {
    Illegal = 0b00,
    Xlen32 = 0b01,
    Xlen64 = 0b10,
    Xlen128 = 0b11,
}

#[bitfield(u26, default = 0)]
pub(crate) struct MisaExt {
    #[bit(0, rw)]
    a: bool,
    #[bit(1, rw)]
    b: bool,
    #[bit(2, rw)]
    c: bool,
    #[bit(3, rw)]
    d: bool,
    #[bit(4, rw)]
    e: bool, // Read-only
    #[bit(5, rw)]
    f: bool,
    #[bit(6, rw)]
    g: bool, // Reserved
    #[bit(7, rw)]
    h: bool,
    #[bit(8, rw)]
    i: bool,
    #[bit(9, rw)]
    j: bool, // Reserved
    #[bit(10, rw)]
    k: bool, // Reserved
    #[bit(11, rw)]
    l: bool, // Reserved
    #[bit(12, rw)]
    m: bool,
    #[bit(13, rw)]
    n: bool, // Reserved
    #[bit(14, rw)]
    o: bool, // Reserved
    #[bit(15, rw)]
    p: bool, // Reserved
    #[bit(16, rw)]
    q: bool,
    #[bit(17, rw)]
    r: bool, // Reserved
    #[bit(18, rw)]
    s: bool,
    #[bit(19, rw)]
    t: bool, // Reserved
    #[bit(20, rw)]
    u: bool,
    #[bit(21, rw)]
    v: bool,
    #[bit(22, rw)]
    w: bool, // Reserved
    #[bit(23, rw)]
    x: bool,
    #[bit(24, rw)]
    y: bool, // Reserved
    #[bit(25, rw)]
    z: bool, // Reserved
}

#[bitfield(u32, default = 0)]
pub(crate) struct Misa32 {
    #[bits(0..=25, rw)]
    extensions: MisaExt,

    #[bits(26..=29, r)]
    wpri: u4,

    #[bits(30..=31, rw)]
    mxl: Mxl,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Misa64 {
    #[bits(0..=25, rw)]
    extensions: MisaExt,

    #[bits(26..=61, r)]
    wpri: u36,

    #[bits(62..=63, rw)]
    mxl: Mxl,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Misa {
    #[bits(0..=31, rw)]
    misa32: Misa32,

    #[bits(0..=63, rw)]
    misa64: Misa64,
}

#[bitfield(u32, default = RAVEN_VENDOR_ID)]
pub(crate) struct Mvendorid {
    #[bits(0..=6, r)]
    _offset: u7,

    #[bits(7..=31, r)]
    _bank: u25,
}

#[bitfield(u64, default = RAVEN_ARCH_ID)]
pub(crate) struct Marchid {
    #[bits(0..=63, r)]
    _archid: u64,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mhartid {
    #[bits(0..=63, r)]
    hartid: u64,
}

#[bitfield(u64, default = RAVEN_IMPL_ID)]
pub(crate) struct Mimpid {
    #[bits(0..=63, r)]
    _impl: u64,
}

#[bitfield(u23, default = 0)]
pub(crate) struct MstatusLowFields {
    #[bits([0, 2, 4] r)]
    wpri: u3,

    #[bit(1, rw)]
    sie: bool,

    #[bit(3, rw)]
    mie: bool,

    #[bit(5, rw)]
    spie: bool,

    #[bit(6, rw)]
    ube: bool,

    #[bit(7, rw)]
    mpie: bool,

    #[bit(8, rw)]
    spp: bool,

    #[bits(9..=10, rw)]
    vs: u2,

    #[bits(11..=12, rw)]
    mpp: u2,

    #[bits(13..=14, rw)]
    fs: u2,

    #[bits(15..=16, rw)]
    xs: u2,

    #[bit(17, rw)]
    mprv: bool,

    #[bit(18, rw)]
    sum: bool,

    #[bit(19, rw)]
    mxr: bool,

    #[bit(20, rw)]
    tvm: bool,

    #[bit(21, rw)]
    tw: bool,

    #[bit(22, rw)]
    tsr: bool,
}

#[bitfield(u32, default = 0)]
pub(crate) struct Mstatus32L {
    #[bits(0..=22, rw)]
    low_fields: MstatusLowFields,

    #[bits(23..=30, r)]
    wpri: u8,

    #[bit(31, rw)]
    sd: bool,
}

#[bitfield(u32, default = 0)]
pub(crate) struct Mstatus32H {
    #[bits([0..=3, 6..=31], r)]
    wpri: u30,

    #[bit(4, rw)]
    sbe: bool,

    #[bit(5, rw)]
    mbe: bool,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mstatus64 {
    #[bits(0..=22, rw)]
    low_fields: MstatusLowFields,

    #[bits([23..=31, 38..=62], r)]
    wpri: u34,

    #[bits(32..=33, rw)]
    uxl: Mxl,

    #[bits(34..=35, rw)]
    sxl: Mxl,

    #[bit(36, rw)]
    sbe: bool,

    #[bit(37, rw)]
    mbe: bool,

    #[bit(63, rw)]
    sd: bool,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mstatus {
    #[bits(0..=31, rw)]
    mstatus32l: Mstatus32L,

    #[bits(32..=63, rw)]
    mstatus32h: Mstatus32H,

    #[bits(0..=63, rw)]
    mstatus64: Mstatus64,
}

#[bitenum(u2, exhaustive = false)]
pub(crate) enum MtvecMode {
    DIRECT = 0b00,
    VECTORED = 0b01,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mtvec {
    #[bits(0..=1, rw)]
    mode: Option<MtvecMode>,

    #[bits(2..=63, rw)]
    base: u62,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Medeleg {
    #[bits(0..=31, rw)]
    medelegl: u32,

    #[bits(32..=63, rw)]
    medelegh: u32,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mideleg {
    #[bits(0..=63, rw)]
    mideleg: u64,
}

#[bitfield(u48, default = 0)]
pub(crate) struct MiCustom {
    #[bits(0..=47, rw)]
    custom: u48,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mip {
    #[bits([0, 2, 3, 6, 8, 10, 12, 14..=15], r)]
    reserved: u9,

    #[bit(1, rw)]
    ssip: bool,

    #[bit(3, rw)]
    msip: bool,

    #[bit(5, rw)]
    stip: bool,

    #[bit(7, rw)]
    mtip: bool,

    #[bit(9, rw)]
    seip: bool,

    #[bit(11, rw)]
    meip: bool,

    #[bit(13, rw)]
    lcofip: bool,

    #[bits(16..=63, r)]
    custom: MiCustom,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mie {
    #[bits([0, 2, 3, 6, 8, 10, 12, 14..=15], r)]
    reserved: u9,

    #[bit(1, rw)]
    ssie: bool,

    #[bit(3, rw)]
    msie: bool,

    #[bit(5, rw)]
    stie: bool,

    #[bit(7, rw)]
    mtie: bool,

    #[bit(9, rw)]
    seie: bool,

    #[bit(11, rw)]
    meie: bool,

    #[bit(13, rw)]
    lcofie: bool,

    #[bits(16..=63, r)]
    custom: MiCustom,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mcycle {
    #[bits(0..=31, rw)]
    mcyclel: u32,

    #[bits(32..=63, rw)]
    mcycleh: u32,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Minstret {
    #[bits(0..=31, rw)]
    minstretl: u32,

    #[bits(32..=63, rw)]
    minstreth: u32,
}

#[bitfield(u32, default = 0)]
pub(crate) struct Mcounteren {
    #[bit(0, rw)]
    cy: bool,

    #[bit(1, rw)]
    tm: bool,

    #[bit(2, rw)]
    ir: bool,

    #[bits(3..=31, r)]
    hp: u29,
}

#[bitfield(u32, default = 0)]
pub(crate) struct Mcountinhibit {
    #[bit(0, rw)]
    cy: bool,

    #[bit(1, rw)]
    tm: bool,

    #[bit(2, rw)]
    ir: bool,

    #[bits(3..=31, r)]
    hp: u29,
}

#[bitfield(u32, default = 0)]
pub(crate) struct Mcause32 {
    #[bits(0..=30, rw)]
    code: u31,

    #[bit(31, rw)]
    interrupt: bool,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mcause64 {
    #[bits(0..=62, rw)]
    code: u63,

    #[bit(63, rw)]
    interrupt: bool,
}

#[bitfield(u64, default = 0)]
pub(crate) struct Mcause {
    #[bits(0..=31, rw)]
    mcause32: Mcause32,

    #[bits(0..=63, rw)]
    mcause64: Mcause64,
}

#[derive(Default)]
pub(crate) struct Csr {
    pub(crate) misa: Misa,
    pub(crate) mstatus: Mstatus,
    pub(crate) mvendorid: Mvendorid,
    pub(crate) marchid: Marchid,
    pub(crate) mimpid: Mimpid,
    pub(crate) mhartid: Mhartid,
    pub(crate) mtvec: Mtvec,
    pub(crate) medeleg: Medeleg,
    pub(crate) mideleg: Mideleg,
    pub(crate) mip: Mip,
    pub(crate) mie: Mie,
    pub(crate) mcycle: Mcycle,
    pub(crate) minstret: Minstret,
    pub(crate) mcounteren: Mcounteren,
    pub(crate) mcountinhibit: Mcountinhibit,
    pub(crate) mscratch: u64,
    pub(crate) mepc: u64,
    pub(crate) mcause: Mcause,
    pub(crate) mtval: u64,
}

impl Cpu {
    fn misa_write_raw(&mut self, val: u64) {
        let tmp = Misa::new_with_raw_value(val);

        // Since MXL is WARL read-only, simply ignore it and only focus on extensions
        let mut tmp_ext = match self.xlen() {
            BaseIsa::RV32I => tmp.misa32().extensions(),
            BaseIsa::RV64I => tmp.misa64().extensions(),
        };

        /* If an attempted write increases IALIGN to 32 (by disabling C) and the next
         * instruction is not IALIGNed, ignore the write completely.
         */
        if !tmp_ext.c() && self.read_pc_next() % 4 != 0 {
            return;
        }

        /* These bits are WARL reserved, and must always return zero on read,
         * so we clear them always before writing back.
         */
        tmp_ext = tmp_ext
            .with_g(false)
            .with_j(false)
            .with_k(false)
            .with_l(false)
            .with_o(false)
            .with_r(false)
            .with_t(false)
            .with_w(false)
            .with_y(false)
            .with_z(false);

        /* These extensions are currently not supported by raven,
         * and the corresponding bits are WARL so we clear them as well.
         */
        tmp_ext = tmp_ext
            .with_b(false)
            .with_h(false)
            .with_n(false)
            .with_p(false)
            .with_q(false)
            .with_v(false)
            .with_x(false);

        /* E is read-only (thus cannot be written directly),
         * but should always be the complement of I.
         * raven currently does not really distinguish between I and E base sets,
         * but keep things consistent for software anyway.
         */
        tmp_ext = tmp_ext.with_e(!tmp_ext.i());

        /* These extensions should only be set if the base set of extensions supports them as well.
         * Technically d and f require base support Zicsr as well, but can never get here unless
         * Zicsr is supported, so don't bother checking for it.
         */
        tmp_ext = tmp_ext
            .with_m(tmp_ext.m() && self.extensions.m())
            .with_a(tmp_ext.a() && self.extensions.a())
            .with_f(tmp_ext.f() && self.extensions.f())
            .with_d(tmp_ext.d() && tmp_ext.f() && self.extensions.d() && self.extensions.f())
            .with_u(tmp_ext.u() && self.extensions.s())
            .with_s(tmp_ext.s() && tmp_ext.u() && self.extensions.s());

        // Finally write back the updated extensions
        match self.xlen() {
            BaseIsa::RV32I => {
                let misa32 = self.reg.csr.misa.misa32().with_extensions(tmp_ext);
                self.reg.csr.misa = self.reg.csr.misa.with_misa32(misa32);
            }
            BaseIsa::RV64I => {
                let misa64 = self.reg.csr.misa.misa64().with_extensions(tmp_ext);
                self.reg.csr.misa = self.reg.csr.misa.with_misa64(misa64);
            }
        }
    }

    fn misa_reset(&mut self) {
        match self.base_isa {
            BaseIsa::RV32I => {
                let misa32 = Misa32::new_with_raw_value(0).with_mxl(Mxl::Xlen32);
                self.reg.csr.misa = Misa::new_with_raw_value(0).with_misa32(misa32);
            }

            BaseIsa::RV64I => {
                let misa64 = Misa64::new_with_raw_value(0).with_mxl(Mxl::Xlen64);
                self.reg.csr.misa = Misa::new_with_raw_value(0).with_misa64(misa64);
            }
        }

        let mxl = match self.base_isa {
            BaseIsa::RV32I => Mxl::Xlen32,
            BaseIsa::RV64I => Mxl::Xlen64,
        };

        let ext = self.extensions;
        let misa_ext = MisaExt::default()
            .with_i(true)
            .with_m(ext.m())
            .with_a(ext.a())
            .with_f(ext.f())
            .with_d(ext.d())
            .with_c(ext.c())
            .with_s(ext.s())
            .with_u(ext.s());

        match self.xlen() {
            BaseIsa::RV32I => {
                let misa32 = Misa32::default().with_mxl(mxl).with_extensions(misa_ext);
                self.reg.csr.misa = Misa::default().with_misa32(misa32);
            }
            BaseIsa::RV64I => {
                let misa64 = Misa64::default().with_mxl(mxl).with_extensions(misa_ext);
                self.reg.csr.misa = Misa::default().with_misa64(misa64);
            }
        }
    }

    fn mstatus_write_raw(&mut self, val: u64) {
        // TODO: For now, simply write bits as is but handle little details later
        match self.xlen() {
            BaseIsa::RV32I => {
                let mstatus32l = Mstatus32L::new_with_raw_value(val as u32);
                self.reg.csr.mstatus = self.reg.csr.mstatus.with_mstatus32l(mstatus32l);
            }
            BaseIsa::RV64I => {
                let mstatus64 = Mstatus64::new_with_raw_value(val);
                self.reg.csr.mstatus = self.reg.csr.mstatus.with_mstatus64(mstatus64);
            }
        }
    }

    fn mstatush_write_raw(&mut self, val: u64) {
        // TODO: For now, simply write bits as-is but handle little details later
        let mstatus32h = Mstatus32H::new_with_raw_value(val as u32);
        self.reg.csr.mstatus = self.reg.csr.mstatus.with_mstatus32h(mstatus32h);
    }

    fn mstatus_reset(&mut self) {
        let mut mstatus = Mstatus::default();

        if self.xlen() == BaseIsa::RV64I && self.ext_supported(Extension::S) {
            let mstatus64 = Mstatus64::default()
                .with_sxl(Mxl::Xlen64)
                .with_uxl(Mxl::Xlen64);
            mstatus = mstatus.with_mstatus64(mstatus64);
        }

        self.reg.csr.mstatus = mstatus;
    }

    fn mtvec_write_raw(&mut self, val: u64) {
        let mtvec = Mtvec::new_with_raw_value(val);
        let base = mtvec.base();
        let mode = mtvec.mode();

        // Base must be 4-byte aligned, otherwise disregard write since WARL field
        if base.value() % 4 == 0 {
            self.reg.csr.mtvec = self.reg.csr.mtvec.with_base(base);
        }

        // Disregard mode if not valid (WARL field)
        if let Ok(mode) = mode {
            self.reg.csr.mtvec = self.reg.csr.mtvec.with_mode(mode);
        }
    }

    fn medeleg_write_raw(&mut self, val: u64) {
        // TODO: Handle read-only zero WARL bits for exceptions that can't be delegated
        match self.xlen() {
            BaseIsa::RV32I => self.reg.csr.medeleg = self.reg.csr.medeleg.with_medelegl(val as u32),
            BaseIsa::RV64I => self.reg.csr.medeleg = Medeleg::new_with_raw_value(val),
        }
    }

    fn medelegh_write_raw(&mut self, val: u64) {
        // TODO: Handle read-only zero WARL bits for exceptions that can't be delegated
        self.reg.csr.medeleg = self.reg.csr.medeleg.with_medelegh(val as u32);
    }

    fn mideleg_write_raw(&mut self, val: u64) {
        // No apparent edge-cases for mideleg, so may remove this function in the future
        self.reg.csr.mideleg = Mideleg::new_with_raw_value(val);
    }

    fn mip_write_raw(&mut self, val: u64) {
        // MEIP, MTIP, and MSIP are read-only to software and set externally
        // Ssscofpmf extension not supported by raven thus lcofip is read-only zero
        let tmp_mip = Mip::new_with_raw_value(val);

        if self.ext_supported(Extension::S) {
            self.reg.csr.mip = self
                .reg
                .csr
                .mip
                .with_seip(tmp_mip.seip())
                .with_stip(tmp_mip.stip())
                .with_ssip(tmp_mip.ssip());
        }
    }

    fn mie_write_raw(&mut self, val: u64) {
        // Ssscofpmf extension not supported by raven thus lcofip is read-only zero
        let tmp_mie = Mie::new_with_raw_value(val);

        self.reg.csr.mie = self
            .reg
            .csr
            .mie
            .with_meie(tmp_mie.meie())
            .with_mtie(tmp_mie.mtie())
            .with_msie(tmp_mie.msie());

        if self.ext_supported(Extension::S) {
            self.reg.csr.mie = self
                .reg
                .csr
                .mie
                .with_seie(tmp_mie.seie())
                .with_stie(tmp_mie.stie())
                .with_ssie(tmp_mie.ssie());
        }
    }

    fn mcycle_write_raw(&mut self, val: u64) {
        match self.xlen() {
            BaseIsa::RV32I => self.reg.csr.mcycle = self.reg.csr.mcycle.with_mcyclel(val as u32),
            BaseIsa::RV64I => self.reg.csr.mcycle = Mcycle::new_with_raw_value(val),
        }
    }

    fn mcycleh_write_raw(&mut self, val: u64) {
        self.reg.csr.mcycle = self.reg.csr.mcycle.with_mcycleh(val as u32);
    }

    fn minstret_write_raw(&mut self, val: u64) {
        match self.xlen() {
            BaseIsa::RV32I => {
                self.reg.csr.minstret = self.reg.csr.minstret.with_minstretl(val as u32)
            }
            BaseIsa::RV64I => self.reg.csr.minstret = Minstret::new_with_raw_value(val),
        }
    }

    fn minstreth_write_raw(&mut self, val: u64) {
        self.reg.csr.minstret = self.reg.csr.minstret.with_minstreth(val as u32);
    }

    fn mcounteren_write_raw(&mut self, val: u64) {
        // hpcounters are currently not supported by raven, so ensure they remain read-only zero
        let tmp = Mcounteren::new_with_raw_value(val as u32);
        self.reg.csr.mcounteren = self
            .reg
            .csr
            .mcounteren
            .with_cy(tmp.cy())
            .with_tm(tmp.tm())
            .with_ir(tmp.ir());
    }

    fn mcountinhibit_write_raw(&mut self, val: u64) {
        // hpcounters are currently not supported by raven, so ensure they remain read-only zero
        // tm is read-only zero
        let tmp = Mcountinhibit::new_with_raw_value(val as u32);
        self.reg.csr.mcountinhibit = self
            .reg
            .csr
            .mcountinhibit
            .with_cy(tmp.cy())
            .with_ir(tmp.ir());
    }

    fn mepc_read_raw(&self) -> u64 {
        // Bit 1 should always appear as cleared when ialign = 32
        match self.ialign() {
            Ialign::I16 => self.reg.csr.mepc,
            Ialign::I32 => self.reg.csr.mepc & !(1 << 1),
        }
    }

    fn mepc_write_raw(&mut self, val: u64) {
        // Bit 0 should always be clear
        self.reg.csr.mepc = val & !1;
    }

    fn mcause_write_raw(&mut self, val: u64) {
        /* MCAUSE is WLRL, so it's the software's responsibility to ensure only legal values
         * are written, as illegal values are implementation-defined. For simplicity we simply
         * allow illegal values to be written.
         */
        self.reg.csr.mcause = Mcause::new_with_raw_value(val);
    }

    fn mtval_write_raw(&mut self, val: u64) {
        /* MTVAL is WARL, but the only stipulation is it must be able to hold all valid
         * addresses. It can optionally hold invalid addresses, thus we just directly write
         * the raw value.
         */
        self.reg.csr.mtval = val;
    }

    fn csr_has_priv(&self, addr: CsrAddr) -> bool {
        if let Ok(mode) = PrivMode::try_from(addr.priv_lvl()) {
            mode == self.priv_mode
        } else {
            // If here, priv lvl in address is reserved, but will be handled by caller
            true
        }
    }

    pub(crate) fn mcause_write(&mut self, code: u64, interrupt: bool) {
        match self.xlen() {
            BaseIsa::RV32I => {
                let mcause32 = Mcause32::new_with_raw_value(code as u32).with_interrupt(interrupt);
                self.reg.csr.mcause = Mcause::default().with_mcause32(mcause32);
            }
            BaseIsa::RV64I => {
                let mcause64 = Mcause64::new_with_raw_value(code).with_interrupt(interrupt);
                self.reg.csr.mcause = Mcause::default().with_mcause64(mcause64);
            }
        }
    }

    pub(crate) fn read_csr(&self, addr: u16) -> Result<u64, CsrError> {
        let csr_addr = CsrAddr::new_with_raw_value(addr);
        if !self.csr_has_priv(csr_addr) {
            return Err(CsrError::InsufficientPrivilege);
        }

        let val = match addr {
            csr_addr::MISA => self.reg.csr.misa.raw_value(),
            csr_addr::MVENDORID => self.reg.csr.mvendorid.raw_value() as u64,
            csr_addr::MARCHID => self.reg.csr.marchid.raw_value(),
            csr_addr::MIMPID => self.reg.csr.mimpid.raw_value(),
            csr_addr::MHARTID => self.reg.csr.mhartid.raw_value(),
            csr_addr::MSTATUS if self.xlen() == BaseIsa::RV32I => {
                self.reg.csr.mstatus.mstatus32l().raw_value() as u64
            }
            csr_addr::MSTATUS if self.xlen() == BaseIsa::RV64I => {
                self.reg.csr.mstatus.mstatus64().raw_value()
            }
            csr_addr::MSTATUSH if self.xlen() == BaseIsa::RV32I => {
                self.reg.csr.mstatus.mstatus32h().raw_value() as u64
            }
            csr_addr::MTVEC => self.reg.csr.mtvec.raw_value(),
            csr_addr::MIDELEG if self.ext_supported(Extension::S) => {
                self.reg.csr.mideleg.raw_value()
            }
            csr_addr::MEDELEG
                if self.ext_supported(Extension::S) && self.xlen() == BaseIsa::RV32I =>
            {
                self.reg.csr.medeleg.medelegl() as u64
            }
            csr_addr::MEDELEG
                if self.ext_supported(Extension::S) && self.xlen() == BaseIsa::RV64I =>
            {
                self.reg.csr.medeleg.raw_value()
            }
            csr_addr::MEDELEGH
                if self.ext_supported(Extension::S) && self.xlen() == BaseIsa::RV32I =>
            {
                self.reg.csr.medeleg.medelegh() as u64
            }
            csr_addr::MIP => self.reg.csr.mip.raw_value(),
            csr_addr::MIE => self.reg.csr.mie.raw_value(),
            csr_addr::MCYCLE => self.reg.csr.mcycle.raw_value(),
            csr_addr::MCYCLEH if self.xlen() == BaseIsa::RV32I => {
                self.reg.csr.mcycle.mcycleh().value() as u64
            }
            csr_addr::MINSTRET => self.reg.csr.minstret.raw_value(),
            csr_addr::MINSTRETH if self.xlen() == BaseIsa::RV32I => {
                self.reg.csr.minstret.minstreth().value() as u64
            }
            csr_addr::MCOUNTEREN if self.ext_supported(Extension::S) => {
                self.reg.csr.mcounteren.raw_value() as u64
            }
            csr_addr::MCOUNTINHIBIT => self.reg.csr.mcountinhibit.raw_value() as u64,
            csr_addr::MSCRATCH => self.reg.csr.mscratch,
            csr_addr::MEPC => self.mepc_read_raw(),
            csr_addr::MCAUSE => self.reg.csr.mcause.raw_value(),
            csr_addr::MTVAL => self.reg.csr.mtval,

            // Not implemented by raven, thus read-only zero
            csr_addr::MCONFIGPTR => 0,
            csr_addr::MENVCFG => 0,
            csr_addr::MENVCFGH if self.xlen() == BaseIsa::RV32I => 0,
            csr_addr::MSECCFG => 0,
            csr_addr::MSECCFGH if self.xlen() == BaseIsa::RV32I => 0,
            csr_addr::MHPMCOUNTER_START..=csr_addr::MHPMCOUNTER_END => 0,
            csr_addr::MHPMCOUNTERH_START..=csr_addr::MHPMCOUNTERH_END
                if self.xlen() == BaseIsa::RV32I =>
            {
                0
            }

            _ => return Err(CsrError::NotSupported),
        };

        Ok(val)
    }

    pub(crate) fn write_csr(&mut self, addr: u16, val: u64) -> Result<(), CsrError> {
        let csr_addr = CsrAddr::new_with_raw_value(addr);

        if !self.csr_has_priv(csr_addr) {
            return Err(CsrError::InsufficientPrivilege);
        }

        if csr_addr.rw() == Rw::Ro {
            return Err(CsrError::WriteToReadOnly);
        }

        match addr {
            csr_addr::MISA => self.misa_write_raw(val),
            csr_addr::MSTATUS => self.mstatus_write_raw(val),
            csr_addr::MSTATUSH if self.xlen() == BaseIsa::RV32I => self.mstatush_write_raw(val),
            csr_addr::MTVEC => self.mtvec_write_raw(val),
            csr_addr::MIDELEG if self.ext_supported(Extension::S) => self.mideleg_write_raw(val),
            csr_addr::MEDELEG if self.ext_supported(Extension::S) => self.medeleg_write_raw(val),
            csr_addr::MEDELEGH
                if self.ext_supported(Extension::S) && self.xlen() == BaseIsa::RV32I =>
            {
                self.medelegh_write_raw(val)
            }
            csr_addr::MIP => self.mip_write_raw(val),
            csr_addr::MIE => self.mie_write_raw(val),
            csr_addr::MCYCLE => self.mcycle_write_raw(val),
            csr_addr::MCYCLEH if self.xlen() == BaseIsa::RV32I => self.mcycleh_write_raw(val),
            csr_addr::MINSTRET => self.minstret_write_raw(val),
            csr_addr::MINSTRETH if self.xlen() == BaseIsa::RV32I => self.minstreth_write_raw(val),
            csr_addr::MCOUNTEREN if self.ext_supported(Extension::S) => {
                self.mcounteren_write_raw(val)
            }
            csr_addr::MCOUNTINHIBIT => self.mcountinhibit_write_raw(val),
            csr_addr::MSCRATCH => self.reg.csr.mscratch = val,
            csr_addr::MEPC => self.mepc_write_raw(val),
            csr_addr::MCAUSE => self.mcause_write_raw(val),
            csr_addr::MTVAL => self.mtval_write_raw(val),

            /* Not implemented by raven, but software should still be able to write
             * without raising an exception so just discard the write.
             */
            csr_addr::MCONFIGPTR => (),
            csr_addr::MENVCFG => (),
            csr_addr::MENVCFGH if self.xlen() == BaseIsa::RV32I => (),
            csr_addr::MSECCFG => (),
            csr_addr::MSECCFGH if self.xlen() == BaseIsa::RV32I => (),
            csr_addr::MHPMCOUNTER_START..=csr_addr::MHPMCOUNTER_END => (),
            csr_addr::MHPMCOUNTERH_START..=csr_addr::MHPMCOUNTERH_END
                if self.xlen() == BaseIsa::RV32I => {}

            _ => return Err(CsrError::NotSupported),
        }

        Ok(())
    }

    pub(crate) fn reset_csr(&mut self) {
        self.reg.csr = Csr::default();
        self.misa_reset();
        self.mstatus_reset();
    }
}
