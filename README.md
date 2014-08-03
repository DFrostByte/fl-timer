fl-timer
========

Countdown timer for Linux written using NASM x86 assembly.


Usage
-----

fl-timer [HH:MM:SS]


Method
------

The command line parameter is validated by ensuring that the colons are
correctly positioned and each pair of digits equate to a value less than 60.

If the parameter fails to validate, the program terminates and outputs a very
simple usage message. If successful, the parameter is copied to writeable RAM
for updating throughout execution of the program. The parameter is prepended
with the '\r' character in preparation for output as this will cause the
current line of output to be overwritten, making it useful for updating the
timer display.

Using Linux kernel interrupts, an interval timer is setup to send a SIGALRM
signal to the process every second, as measured against the real system clock.
A function is setup as a signal handler to decrement the timer whenever the
SIGALRM signal is received.

After the setup, the process enters a loop of outputting the timer then halting
until a signal is received.

The process terminates when the signal handler finds the timer to have reached
00:00:00.


###  Timer Algorithm ###

Are there any seconds that can be decremented? If not, try minutes then hours.
If the hours or minutes are decremented, the minutes and seconds are reset to
59 respectively.



Build
-----

nasm -f elf -o start.o start.asm && ld -s -o fl-timer start.o
