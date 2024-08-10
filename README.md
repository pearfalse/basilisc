# Basilisc

Basilisc is a converter for BBC BASIC files that can convert between plaintext and tokenised forms. It boasts the following features:

- Supports files from 8-bit (BBC Micro/Master) and 32-bit (Archimedes / RiscPC) Acorn machines.
- Handles line numbers and line number references properly.
- Only prints line numbers for lines that are referenced (or for all lines, if you prefer). Can pack textual BASIC files with full, partial, or no line number definitions.
- Properly converts non-ASCII characters in string literals and C0 control codes to printable Unicode characters, and back again.
- Correctly unpacks squashed BASIC files without corrupting tokens by adding in spaces where they're needed. (If adding in those spaces made the line too long to re-pack, it will remove those spaces on packing.)

Basilisc is still in development, and may fail on some extra-weird BASIC files. Please open an issue if you notice something it does wrong.

There is no \[deliberate\] support for post-Acorn BASIC files. I'm not opposed to supporting them, I just have no use case for it myself.

## Building from source

To generate a release binary from source:

- Clone the repo to a folder of your choosing (the default branch is `trunk`).
- Run `build.sh` from a terminal `cd`-ed to the repo root. This requires a unix-like environment; if on Windows, run `build.cmd` instead.
- The binary is at `target/release/basilisc` (`basilisc.exe` on Windows).
