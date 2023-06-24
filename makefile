release:
	cargo build --release
# Phony
.PHONY: release

debug:
	cargo build
# Phony
.PHONY: debug

target-release:
	cross build --release --target $(triple)
# Phony
.PHONY: target-release

target-debug:
	cross build --target $(triple)
# Phony
.PHONY: target-debug

cbindgen:
	cbindgen --config cbindgen.toml --crate craneliftc --output ./headers/craneliftc.h
# Phony
.PHONY: cbindgen

