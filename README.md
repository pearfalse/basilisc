# Basilisc

Basilisc is a converter for BBC BASIC files that can convert between plaintext and tokenised forms. It boasts the following features:

- Supports files from 8-bit (BBC Micro/Master) and 32-bit (Archimedes / RiscPC) Acorn machines.
- Handles line numbers and line number references properly.
- Only prints line numbers for lines that are referenced (or for all lines, if you prefer). Can pack textual BASIC files with full, partial, or no line number definitions.
- Properly converts non-ASCII characters in string literals and C0 control codes to printable Unicode characters, and back again.
- Correctly unpacks squashed BASIC files without corrupting tokens by adding in spaces where they're needed. (If adding in those spaces made the line too long to re-pack, it will remove those spaces on packing.)

Basilisc is still in development, and may fail on some extra-weird BASIC files. Please open an issue if you notice something it does wrong.

There is no \[deliberate\] support for post-Acorn BASIC files. I'm not opposed to supporting them, I just have no use case for it myself.

## Usage

To unpack a tokenised BASIC file:

<code>basilisc unpack -i path/to/basic/file,ffb -o path/to/text/version.txt</code>

The output will be a UTF-8 text file with LF line endings with the full program listing. Line numbers are only included for any lines referenced with a `GOTO` or `GOSUB`; if you want all lines to have their line numbers included, add the argument `--use-line-numbers=always` to the command. Non-printing and non-ASCII characters are preserved with a lossless mapping to printing Unicode characters.

To convert the other way:

<code>basilisc pack -i path/to/text/version.txt -o path/to/basic/file,ffb</code>

The output will be a tokenised BASIC file ready to use on any version of BBC BASIC.

For more help on available command options, run `basilisc pack --help` or `basilisc unpack --help`.

## Round-trip conversion

Basilisc is designed to support round-trip conversion, with as close to a byte-for-byte preservation as is reasonably possible. Reasons for discrepancies might include:

- The `unpack` command didn't ask for all line numbers, or some were removed from the text version, and Basilisc filled in different numbers.
- The tokenised BASIC file was squashed, which often removes space characters that need to be present in text versions of the same code. Basilisc will add those in when `unpack`-ing anywhere it needs to. It'll only remove them again if that's what it takes to bring a long line under the hard limit of 251 bytes.

## Building from source

To generate a release binary from source:

- Clone the repo to a folder of your choosing (the default branch is `trunk`).
- Run `build.sh` from a terminal `cd`-ed to the repo root. This requires a unix-like environment; if on Windows, run `build.cmd` instead.
- The binary is at `target/release/basilisc` (`basilisc.exe` on Windows).
