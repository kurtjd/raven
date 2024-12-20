use crate::cpu::*;

#[derive(Debug, Copy, Clone)]
pub(crate) enum Trap {
    InstructionAddressMisaligned,
    InstructionAccessFault,
    IllegalInstruction,
    Breakpoint,
    _LoadAddressMisaligned,
    LoadAccessFault,
    _StoreAddressMisaligned,
    StoreAccessFault,
    EnvironmentCallFromU,
    EnvironmentCallFromS,
    EnvironmentCallFromM,
    _InstructionPageFault,
    _LoadPageFault,
    _StorePageFault,
    _SoftwareCheck,
    _HardwareError,
}

impl From<Trap> for u64 {
    fn from(t: Trap) -> Self {
        match t {
            Trap::InstructionAddressMisaligned => 0,
            Trap::InstructionAccessFault => 1,
            Trap::IllegalInstruction => 2,
            Trap::Breakpoint => 3,
            Trap::_LoadAddressMisaligned => 4,
            Trap::LoadAccessFault => 5,
            Trap::_StoreAddressMisaligned => 6,
            Trap::StoreAccessFault => 7,
            Trap::EnvironmentCallFromU => 8,
            Trap::EnvironmentCallFromS => 9,
            Trap::EnvironmentCallFromM => 11,
            Trap::_InstructionPageFault => 12,
            Trap::_LoadPageFault => 13,
            Trap::_StorePageFault => 15,
            Trap::_SoftwareCheck => 18,
            Trap::_HardwareError => 19,
        }
    }
}

impl From<Trap> for u32 {
    fn from(t: Trap) -> Self {
        u64::from(t) as u32
    }
}

#[derive(Debug, Copy, Clone)]
pub enum _Interrupt {
    SupervisorSoftware,
    MachineSoftware,
    SupervisorTimer,
    MachineTimer,
    SupervisorExternal,
    MachineExternal,
    CounterOverflow,
}

impl Cpu {
    pub(crate) fn trap(&mut self, trap: Trap) {
        // TODO: Handle delegations

        // Store the cause of the exception
        self.mcause_write(u64::from(trap), false);

        // Store the current pc
        self.reg.csr.mepc = self.read_pc();

        // Store additional info about trap
        // TODO: Store relevant info, but for now store 0
        self.reg.csr.mtval = 0;

        // Jump to the vector base-address (always 4 byte aligned hence left shift)
        // Traps always jump to base, regardless of mode
        let base = u64::from(self.reg.csr.mtvec.base()) << 2;
        self.write_pc_next(base);

        println!("TRAP: {:?}", trap);
        println!("PC: 0x{:X}", self.read_pc());
        println!("Next PC: 0x{:X}", self.read_pc_next());
    }

    pub(crate) fn _interrupt(&mut self, interrupt: _Interrupt) {
        todo!("Interrupt: {:?}", interrupt);
    }
}
