#!/bin/sh

cargo run -p token-data-gen

RUSTFLAGS="\
--remap-path-prefix $HOME/.cargo/registry=/cargo-registry \
--remap-path-prefix $(pwd)=/basilisc \
" exec cargo build --release
