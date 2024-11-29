use raven::vm::VirtualMachine;
use std::env;

const MEM_SZ: usize = 4 * (1024) * (1024);

fn main() {
    let args: Vec<String> = env::args().collect();
    let bin = &args[1];

    let mut vm = VirtualMachine::new("RV32I", MEM_SZ).unwrap();
    vm.load_binary(bin).unwrap();
    vm.start();
}
