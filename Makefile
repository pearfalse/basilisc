META = meta-src/token_data.rs

RUSTFLAGS = --remap-path-prefix $(HOME)/.cargo/registry=/cargo-registry
RUSTFLAGS += --remap-path-prefix $$(pwd)=/basilisc

$(META):
	cargo run -p token-data-gen

.PHONY: install

debug: $(META)
	cargo build

release: $(META)
	RUSTFLAGS="$(RUSTFLAGS)" cargo build --release

install: release
	cp target/release/basilisc $(HOME)/bin/
