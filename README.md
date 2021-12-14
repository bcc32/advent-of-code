# advent-of-code.el

This package provides some convenient features for solving Advent of
Code problems quickly.

Press `C-c C-c` in an input buffer (`aoc.in`) to re-download the
actual input assigned to you, in case you edited the file for
debugging purposes.

After the output buffer (`aoc.out`) is written to and auto-reverted by
Emacs, its contents are automatically copied into the kill ring (and
the X clipboard, if supported).
