use raven::vm::VirtualMachine;
use std::env;

const MEM_SZ: usize = 4 * (1024) * (1024);

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: raven <isa> <bin>");
        return;
    }

    let isa = &args[1];
    let bin = &args[2];

    let mut vm = VirtualMachine::new(isa, MEM_SZ).unwrap();
    vm.load_binary(bin).unwrap();
    vm.start();
}
