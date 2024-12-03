use crate::cpu::*;

pub(crate) enum Trap {
    InstructionAddressMisaligned,
    InstructionAccessFault,
    IllegalInstruction,
    Breakpoint,
    LoadAddressMisaligned,
    LoadAccessFault,
    StoreAddressMisaligned,
    StoreAccessFault,
    EnvironmentCallFromU,
    EnvironmentCallFromS,
    EnvironmentCallFromM,
    InstructionPageFault,
    LoadPageFault,
    StorePageFault,
    SoftwareCheck,
    HardwareError,
}

pub enum Interrupt {
    SupervisorSoftware,
    MachineSoftware,
    SupervisorTimer,
    MachineTimer,
    SupervisorExternal,
    MachineExternal,
    CounterOverflow,
}

impl Cpu {
    pub(crate) fn trap(&mut self, trap: Trap) {}
    pub(crate) fn interrupt(&mut self, interrupt: Interrupt) {}
}
