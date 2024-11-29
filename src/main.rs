use raven::vm::VirtualMachine;

fn main() {
    let mut vm = VirtualMachine::new("RV32I", 8192).unwrap();
    vm.load_binary("bin/hello_world.bin").unwrap();
    vm.start();
}
