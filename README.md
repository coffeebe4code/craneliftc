# Cranelift C Compatible FFI

## LICENSE
I do not maintain cranelift, or any part of wasmtime.
This is merely a redistribution of the abi in a c compatible way.

Here is the Source's license.
[wasmtime license](https://github.com/bytecodealliance/wasmtime/blob/main/LICENSE)

From what I can tell in the license, this is not necessarily a Derivative work, but I am treating it as a Derivative work, and being careful to shove this license in your face everywhere.

## Crate

**Note** to users seeing this on crates.io, you will need to follow the [To Use](#To-Use) documentation to install.
This is a greate place to see the abi's on a different screen.

Alternately, you can create a wrapper project, and `pub use` both craneliftc, and cranelift, and run cargo build for that project. It will then get installed to your local target directory, and then use the `cbindgen command found in the makefile`. These are significant extra steps. Installing with the repsitory locally is easier.

## To Use
 
1. ensure you are on rust nightly.
2. ensure you have cbindgen installed. `cargo install cbindgen`

**Note** if you want to cross compile `craneliftc`, ensure you have cross installed. `cargo install cross`

You can run any of these in this repositories root directory.
-  `make release`, 
-  `make debug`, 
-  `make triple=${your_target} target-release` 
-  `make triple=${your_target} target-debug` 

This installs `craneliftc` into the `./target` directory for either your build or release version. If you provided a cross-target that will be installed with an additional directory for the triple first.

This library uses opaque pointers, so an additional header file is needed. `craneliftc-extras.h`

As a convenience, the header files can be taken from here.
[header files](https://github.com/coffeebe4code/craneliftc/tree/main/headers)

