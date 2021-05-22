# uLisp Builder
Builds a version of uLisp for a particular platform from a common repository of source files.

Currently supports the AVR, ARM, ESP8266/32, and RISC-V platforms.

For information see http://www.ulisp.com/show?3F07.

# Using

Instructions:

1. Start REPL.

2. Push desired platform to `*features*`:

   `(push :badge *features*)`

   See header of `Load Builder.lisp` for full list of platforms.

3. Load builder:

   `(load "Load Builder")`

4. Run builder:

   `(build :badge)`

Builder will create `ulisp.ino` alongside `Load Builder.lisp`.
