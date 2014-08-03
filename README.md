fl-timer
========

Countdown timer for Linux written using NASM x86 assembly.


Usage
-----

fl-timer [HH:MM:SS]


Method
------

The command line parameter is validated by ensuring that the colons are correctly
positioned

Build
-----

nasm -f elf -o start.o start.asm && ld -s -o fl-timer start.o
