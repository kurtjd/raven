#[non_exhaustive]
#[derive(Debug)]
pub enum MemoryError {
    Invalid,
    OutOfRange,
    Misaligned,
}

pub trait MemoryAccess {
    fn loadb(&self, addr: u64) -> Result<u8, MemoryError>;
    fn loadh(&self, addr: u64) -> Result<u16, MemoryError>;
    fn loadw(&self, addr: u64) -> Result<u32, MemoryError>;
    fn loadd(&self, addr: u64) -> Result<u64, MemoryError>;

    fn storeb(&mut self, addr: u64, val: u8) -> Result<(), MemoryError>;
    fn storeh(&mut self, addr: u64, val: u16) -> Result<(), MemoryError>;
    fn storew(&mut self, addr: u64, val: u32) -> Result<(), MemoryError>;
    fn stored(&mut self, addr: u64, val: u64) -> Result<(), MemoryError>;

    fn clear(&mut self);
}
