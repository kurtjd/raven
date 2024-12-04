use crate::cpu::*;

#[derive(Debug)]
pub(crate) enum Trap {
    _InstructionAddressMisaligned,
    _InstructionAccessFault,
    IllegalInstruction,
    _Breakpoint,
    _LoadAddressMisaligned,
    _LoadAccessFault,
    _StoreAddressMisaligned,
    _StoreAccessFault,
    _EnvironmentCallFromU,
    _EnvironmentCallFromS,
    _EnvironmentCallFromM,
    _InstructionPageFault,
    _LoadPageFault,
    _StorePageFault,
    _SoftwareCheck,
    _HardwareError,
}

#[derive(Debug)]
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
        todo!("Trap: {:?}", trap);
    }

    pub(crate) fn _interrupt(&mut self, interrupt: _Interrupt) {
        todo!("Interrupt: {:?}", interrupt);
    }
}
