# Aiden's Small C Compiler

I'm writing a C89 compiler.

I started using the original ANSI document that has all the fun details, but
found it easier to just use the GNU C manual. I'm aware it also covers extensions supported by GCC, but I've just left those out.
https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html

## Tests

Tests are found in `lib/test.ml`. I'm using the [expect test format](https://github.com/janestreet/ppx_expect) from Jane Street, which I've found very useful.

Code coverage will be listed at a later date.
