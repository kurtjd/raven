use raven::*;
use rstest::*;
use std::path::{Path, PathBuf};

const MEM_SZ: usize = 4 * 1024 * 1024;

fn op_test(isa: &str, path: &Path) {
    let mut vm = vm::VirtualMachine::new(isa, MEM_SZ).unwrap();
    vm.load_binary(path.to_str().unwrap()).unwrap();

    let status = vm.start();
    assert_eq!(status, 0);
}

#[rstest]
fn test_32ui(#[files("tests/bin/rv32ui-p-*")] path: PathBuf) {
    op_test("RV32I_Zicsr", &path);
}

#[rstest]
fn test_64ui(#[files("tests/bin/rv64ui-p-*")] path: PathBuf) {
    op_test("RV64I_Zicsr", &path);
}

#[rstest]
fn test_32mi(#[files("tests/bin/rv32mi-p-*")] path: PathBuf) {
    op_test("RV32I_Zicsr", &path);
}

#[rstest]
fn test_64mi(#[files("tests/bin/rv64mi-p-*")] path: PathBuf) {
    op_test("RV64I_Zicsr", &path);
}

#[rstest]
fn test_32um(#[files("tests/bin/rv32um-p-*")] path: PathBuf) {
    op_test("RV32IM_Zicsr", &path);
}

#[rstest]
fn test_64um(#[files("tests/bin/rv64um-p-*")] path: PathBuf) {
    op_test("RV64IM_Zicsr", &path);
}
