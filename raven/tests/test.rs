use raven::*;
use rstest::*;
use std::path::PathBuf;

const MEM_SZ: usize = 4 * 1024 * 1024;

fn op_test(isa: &str, path: PathBuf) {
    let mut vm = vm::VirtualMachine::new(isa, MEM_SZ).unwrap();
    vm.load_binary(path.to_str().unwrap()).unwrap();

    let status = vm.start();
    assert_eq!(status, 0);
}

#[rstest]
fn test_32ui(#[files("tests/bin/rv32ui-p-*")] path: PathBuf) {
    op_test("RV32I", path);
}
