use crate::{
    cpu::{Cpu, IsaError},
    memory::{MemoryAccess, MemoryError},
};
use std::fs::File;
use std::io::{self, Read};

const RESET_ADDR: u64 = 0x8000_0000;
const PUTC_ADDR: u64 = 0x1000_0000;
const PUTI_ADDR: u64 = 0x1000_0001;
const RAM_START: u64 = 0x8000_0000;
const RAM_END: u64 = u64::MAX;

const RETURN_REG: u8 = 10;

macro_rules! to_ram_idx {
    ($addr:expr) => {
        (($addr - RAM_START) as usize)
    };
}

struct MemoryController {
    ram: Vec<u8>,
    memsz: usize,
}

impl MemoryController {
    fn new(memsz: usize) -> Self {
        Self {
            ram: Vec::with_capacity(memsz),
            memsz,
        }
    }
}

impl MemoryAccess for MemoryController {
    fn loadb(&self, addr: u64) -> Result<u8, MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz {
                    Ok(self.ram[idx])
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn loadh(&self, addr: u64) -> Result<u16, MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 1 {
                    let bytes = &self.ram[idx..idx + 2];
                    let bytes: [u8; 2] = bytes.try_into().unwrap();
                    Ok(u16::from_le_bytes(bytes))
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn loadw(&self, addr: u64) -> Result<u32, MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 3 {
                    let bytes = &self.ram[idx..idx + 4];
                    let bytes: [u8; 4] = bytes.try_into().unwrap();
                    Ok(u32::from_le_bytes(bytes))
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn loadd(&self, addr: u64) -> Result<u64, MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 7 {
                    let bytes = &self.ram[idx..idx + 8];
                    let bytes: [u8; 8] = bytes.try_into().unwrap();
                    Ok(u64::from_le_bytes(bytes))
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn storeb(&mut self, addr: u64, val: u8) -> Result<(), MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz {
                    self.ram[idx] = val;
                    Ok(())
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            PUTC_ADDR => {
                print!("{}", char::from(val));
                Ok(())
            }
            PUTI_ADDR => {
                println!("{val}");
                Ok(())
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn storeh(&mut self, addr: u64, val: u16) -> Result<(), MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 1 {
                    let bytes = val.to_le_bytes();
                    self.ram[idx..idx + 2].copy_from_slice(&bytes);
                    Ok(())
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn storew(&mut self, addr: u64, val: u32) -> Result<(), MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 3 {
                    let bytes = val.to_le_bytes();
                    self.ram[idx..idx + 4].copy_from_slice(&bytes);
                    Ok(())
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn stored(&mut self, addr: u64, val: u64) -> Result<(), MemoryError> {
        match addr {
            RAM_START..=RAM_END => {
                let idx = to_ram_idx!(addr);
                if idx < self.memsz - 7 {
                    let bytes = val.to_le_bytes();
                    self.ram[idx..idx + 8].copy_from_slice(&bytes);
                    Ok(())
                } else {
                    Err(MemoryError::OutOfRange)
                }
            }
            _ => Err(MemoryError::Invalid),
        }
    }

    fn clear(&mut self) {
        self.ram.clear();
    }
}

pub struct VirtualMachine {
    cpu: Cpu,
    mc: MemoryController,
}

impl VirtualMachine {
    pub fn new(isa: &str, memsz: usize) -> Result<Self, IsaError> {
        let cpu = Cpu::new(isa, RESET_ADDR)?;
        let mc = MemoryController::new(memsz);

        Ok(Self { cpu, mc })
    }

    pub fn start(&mut self) -> u64 {
        self.cpu.halted = false;

        while !self.cpu.halted {
            self.cpu.step(&mut self.mc);
        }

        self.cpu.read_gpr(RETURN_REG)
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.mc.clear();
    }

    pub fn load_binary(&mut self, path: &str) -> io::Result<()> {
        let mut bin = File::open(path)?;
        let binsz = bin.metadata()?.len() as usize;

        if binsz <= self.mc.ram.capacity() {
            bin.read_to_end(&mut self.mc.ram)?;
            Ok(())
        } else {
            Err(io::ErrorKind::InvalidInput.into())
        }
    }
}
