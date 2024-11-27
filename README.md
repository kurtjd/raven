# Raven
A RISC-V emulator written in Rust supporting RV32GCS and RV64GCS.

## Features
Raven aims to emulate the following, with the goal of supporting a modern operating system:
- Both RV32I and RV64I base ISAs
- The general-purpose "G" extension, shorthand for:
    - The multiplication/division instructions "M" extension
    - The atomic instructions "A" extension
    - The single-precision floating-point "F" extension
    - The double-precision floating-point "D" extension
    - The CSR instructions "Zicsr" extension
    - The instruction-fetch fence "Zifencei" extension
- The compressed instructions "C" extension
- The supervisor mode "S" extension

Additionally, Raven contains a lightweight virtual machine for handling memory management and
emulated device/peripheral I/O.

## Build
TBD

## Run
TBD

## TODO
- Implement RV32I
- Implement RV64I
- Implement "M" extension
- Implement "Zicsr" extension
- Implement "F" extension
- Implement "D" extension
- Implement "A" extension
- Implement "Zifencei" extension
- Implement "C" extension
- Implement machine-mode privileged ISA and CSRs
- Implement machine-mode exception/trap handling
- Implement "S" extension
- Implement supervisor-mode privileged ISA and CSRs
- Implement supervisor-mode exception/trap handling
- Implement multiple harts (maybe, but would be good for testing atomics)

## FAQ
### What does this do differently/better than other RISC-V emulators?
Nothing.

### So why make it then?
As Zero Cool once so elegantly put it: "RISC is good."  
And because I want to thoroughly understand how a modern ISA supports operating systems for when
I attempt to write my own.

### Why is it called "Raven"?
The R is for Rust/RISC and the V is for, well, RISC-V.
Also, it's the name of a character from one of my favorite books **Snow Crash** and just sounds cool.

## License
Raven is licensed under the MIT license and is completely free to use and modify.