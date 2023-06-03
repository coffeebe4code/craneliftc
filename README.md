# Cranelift C Compatible FFI

## LICENSE
I do not maintain cranelift, or any part of wasmtime.
This is merely a redistribution of the abi in a c compatible way.

Here is the Source's license.
[wasmtime license](https://github.com/bytecodealliance/wasmtime/blob/main/LICENSE)

From what I can tell in the license, this is not necessarily a Derivative work, but I am treating it as a Derivative work, and being careful to shove this license in your face everywhere.

## To Use
install this crate.
 
You can then install and run mozilla's `cbindgen` in order to generate the header files.
As this project uses opaque pointers to cranelifts structs, an additional header file with the types defined is necessary.

As a convenience, the header files can be taken from here.
[header files](https://github.com/coffeebe4code/craneliftc/tree/main/headers)

If you are installing the latest crate of craneliftc, the headers are completely in sync.

updating your version of craneliftc will require copying the header files again from source.




