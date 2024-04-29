# Basilisc

Basilisc is a converter for BBC BASIC files that can convert between plaintext and tokenised forms. It boasts the following features:

- Supports files from 8-bit (BBC Micro/Master) and 32-bit (Archimedes / RiscPC) Acorn machines.
- Handles line numbers and line number references properly.
- Only prints line numbers for lines that are referenced (or all lines, if you prefer).
- Properly converts control codes or non-ASCII characters in string literals to printable Unicode
  characters, and back again.
- Correctly unpacks squashed BASIC files by adding in spaces where they're needed.

Basilisc is still in development, and may fail on some BASIC files. Please open an issue if you notice something it does wrong.

There is no \[deliberate\] support for post-Acorn BASIC files. I'm not opposed to supporting them, I just have no use case for it myself.
