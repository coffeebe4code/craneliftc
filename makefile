release: cbindgen
	cargo build --release
# Phony
.PHONY: release

debug: cbindgen
	cargo build
# Phony
.PHONY: debug

target-release: cbindgen
	cross build --release --target $(triple)
# Phony
.PHONY: target-release

target-debug: cbindgen
	cross build --target $(triple)
# Phony
.PHONY: target-debug

cbindgen:
	cbindgen --config cbindgen.toml --crate craneliftc --output ./headers/craneliftc.h
# Phony
.PHONY: cbindgen

