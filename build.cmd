cargo.exe run -p token-data-gen

set RUSTFLAGS="--remap-path-prefix %HOMEDRIVE%%HOMEPATH%\.cargo\registry=/cargo-registry --remap-path-prefix %CD%=/src"
cargo.exe build --release
