;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAM: fl-timer
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESCRIPTION: Read user given time from the command line arguments, supplied
;;   in the following format: HH:MM:SS.
;;   For the duration of the given time, show a countdown timer and stopwatch,
;;   displaying remaining and elapsed time respectively, with seconds
;;	 precision.
;; CREATED: 2014-07-17
;; MODIFIED: 2014-07-28
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; METHOD:
;	Confirm argument is provided.
;	Validate format
;	Calculate total number of seconds
;		SS + (MM * [SS per min]) + (HH * [SS per hour])
;	Loop every second until no time remaining:
;		Display time remaining and time elapsed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%define _KERNEL_INTERRUPT 0x80

%define _SYS_CALL_WRITE 4
%define _SYS_CALL_EXIT  1
%define 	_EXIT_CODE_SUCCESS 0
%define 	_EXIT_CODE_ERROR   1
%define _SYS_CALL_SIGACTION 0x43
%define 	_SIGALRM 14 ; SIGALRM
%define _SYS_CALL_PAUSE 0x1d
%define _SYS_CALL_SETITIMER 0x68
%define 	_ITIMER_REAL 0

%define _STDIN  0
%define _STDOUT 1
%define _STDERR 2

%define _ARG_COUNT 2               ; 2 = exe + time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variables / definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[SECTION .data]

_str_newline: db `\n`

; storage of the timer that will be output
_str_timer:       db `\r`          ; when output without '\n', clears current
_str_timer_hours: db '00:'
_str_timer_mins:  db '00:'
_str_timer_secs:  db '00'
_STR_TIMER_BYTES equ $ - _str_timer

; user param should be the same length minus the '/r' byte
_USER_PARAM_LENGTH equ _STR_TIMER_BYTES - 1
;~ struct sigaction {
               ;~ void     (*sa_handler)(int);
               ;~ void     (*sa_sigaction)(int, siginfo_t *, void *);
               ;~ sigset_t   sa_mask;
               ;~ int        sa_flags;
               ;~ void     (*sa_restorer)(void);
           ;~ };
_struct_sigaction: dd _f_timer_update, 0, 0, 0, 0
_struct_itimerval: dd 1, 0, 1, 0
	; set timer interval timer duration
	;~ dd 1                           ; struct timeval secs
	;~ dd 0                           ; struct timeval nano secs
	;~ ; next expiration
	;~ dd 1                           ; struct timeval secs
	;~ dd 0                           ; struct timeval nano secs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; variable declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[SECTION .bss]
	 ; memory in this section can be reserved but not defined. upon program
	 ; entry, all memory in this section will be initialised as null bytes.
;~ _.string: resb 11

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
[SECTION .text]

global _start
;~ global _f_timer_update

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; program entry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_start:
	cmp dword [esp], _ARG_COUNT
	jne _error_args

	mov dword esi, [esp+2*4]       ; ESI <- first user arg (timer)
    call _f_validate_user_timer
    test eax, eax
    jnz _error_args

	; copy 8-byte parameter to storage for updating and outputting later
	mov dword edi, _str_timer_hours
	movsd
	movsd

	; setup signal handler
	; the function address stored in _struct_sigaction will be called when
	; the SIGALRM signal is received
	mov dword eax, _SYS_CALL_SIGACTION
	mov dword ebx, _SIGALRM
	mov dword ecx, _struct_sigaction     ; *sigaction struct (new handler)*
	mov dword edx, 0                     ; *storage old sigaction (null)*
	int _KERNEL_INTERRUPT

	; setup interval timer (sends SIGALRM) for signal handler
	mov dword eax, _SYS_CALL_SETITIMER
	mov dword ebx, _ITIMER_REAL          ; use real time clock
	mov dword ecx, _struct_itimerval     ; interval timer settings
	mov dword edx, 0                     ; don't store old timer value
	int _KERNEL_INTERRUPT
	test eax, eax
	jnz _exit

.loop_timer:
	; output timer
	mov dword eax, _SYS_CALL_WRITE
	mov dword ebx, _STDOUT
	mov dword ecx, _str_timer
	mov dword edx, _STR_TIMER_BYTES
	int _KERNEL_INTERRUPT

	; sys_pause (stop execution and wait for signal)
	mov dword eax, _SYS_CALL_PAUSE
	int _KERNEL_INTERRUPT

	; execution continues here after signal handler (_f_timer_update)
	jmp .loop_timer



_exit:
	; output a newline
	mov dword eax, _SYS_CALL_WRITE
	mov dword ebx, _STDOUT
	mov dword ecx, _str_newline
	mov dword edx, 1               ; number of bytes to write
	int _KERNEL_INTERRUPT

	; exit program
	mov dword eax, _SYS_CALL_EXIT
	mov dword ebx, _EXIT_CODE_SUCCESS
	int _KERNEL_INTERRUPT

_error_args:
	; output usage information
	mov dword eax, _SYS_CALL_WRITE
	mov dword ebx, _STDERR
	mov dword ecx, .usage_msg
	mov dword edx, .USAGE_MSG_BYTES
	int _KERNEL_INTERRUPT

	jmp _exit

.usage_msg: db 'usage: [HH:MM:SS]', `\n`
.USAGE_MSG_BYTES equ $ - .usage_msg

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function: _f_validate_user_timer
; Description:
;   validate the user's timer provided on the command line
; Parameters:
;	ESI: users string address
; Returns:
;	EAX: 0 if validated, else invalid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_f_validate_user_timer:
	push esi
	push edi
	push ecx

	; length should be 8 (00:00:00)
	mov edi, esi
	call _f_get_string_length
	cmp eax, _USER_PARAM_LENGTH
	jne .error                     ; string is different length

	mov dword ecx, 3               ; number of times to loop - 3 pairs digits
	; start by processing first two bytes, which should be numeric
	jmp .loop_digits
.loop_seperator:
	; check next byte for seperator (':')
	lodsb
	cmp byte al, ':'
	jne .error
.loop_digits:
	; each pair of digits should be less than 60
	lodsw                          ; AL <- [ESI], AH <- [ESI+1], INC ESI

	cmp byte al, '5'
	jg .error
	cmp byte al, '0'
	jl .error

	cmp byte ah, '9'
	jg .error
	cmp byte ah, '0'
	jl .error                 ; less than 0

	loop .loop_seperator
	; end of loop - parameter should be valid

	xor eax, eax                   ; EAX <- 0 == success

.leave:
	pop ecx
	pop edi
	pop esi

	ret

.error:
	mov dword eax, 1
	jmp .leave


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function: _f_timer_update
; Description:
;   Signal handler for updating timer
; Parameters:
;   Stack (push order):
;		1: (dword) signal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_f_timer_update:

	; decrement seconds
	lea edi, [_str_timer_secs]     ; tens of seconds byte
	call _f_dec_ascii_99
	test eax, eax                  ; was the value decremented?
	jnz .decremented               ; yes - done.

	; seconds couldn't be decremented, try minutes
	lea edi, [_str_timer_mins]     ; tens of minutes byte
	call _f_dec_ascii_99
	test eax, eax                  ; were the minutes decremented?
	jnz .set_seconds               ; yes, so reset seconds to 59

	; minutes couldn't be decremented, try hours
	lea edi, [_str_timer_hours]    ; tens of hours byte
	call _f_dec_ascii_99
	test eax, eax
	jz .timer_end     ; couldn't even take time from the hours, so the timer
	                  ; must have completed.
	; the hours were decremented. reset minutes and seconds to 59
	; little-endian. tens in low byte to be first byte in memory
	mov word [_str_timer_mins], 0x3935    ; [5][9]
.set_seconds:
	mov word [_str_timer_secs], 0x3935    ; [5][9]

.decremented:
	ret

.timer_end:
	jmp _exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function: _f_dec_ascii_99
; Description:
;   decrement a pair of bytes that represent tens and units
; Parameters:
;   EDI: address of tens bytes
; Return:
;	EAX: 0 if can't be decremented (both bytes '0')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_f_dec_ascii_99:
	xor eax, eax                   ; clear EAX - prep return

	mov word ax, [edi]             ; load both string bytes in ax
	xor ax, 0x3030                 ; set bytes to binary from ascii digits

	test ah, ah                    ; any units?
	jnz .dec_units                 ; yes, decrement
	test al, al                    ; no, so any tens?
	jz .end                        ; no, return

.dec_tens:
	dec al                         ; yes, decrement then...
	mov ah, 10                     ; set units to 9 (decremented below)
.dec_units:
	dec ah

	; save
	or ax, 0x3030                  ; convert back to ascii characters
	mov word [edi], ax             ; copy back to memory

.end:
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function: _f_get_string_length
; Description:
;   find byte-length of given null-terminated string.
; Parameters:
;   EDI: string address
; Returns:
;	EAX: string length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_f_get_string_length:
	%assign __MAX_SCAN_BYTES 0x00_00_00_FF

	; preserve registers
	push edi
	push ecx

	; scan string byte by byte starting at address in EDI looking for value in
	; AL. after scan, EDI should be address of AL value found (null byte).
	xor eax, eax                   ; clear AL (searching for null-byte)
	cld                            ; scan up memory (inc EDI)
	mov ecx, __MAX_SCAN_BYTES      ; limit loop
	repne scasb                    ; loop: [EDI] <> AL. inc EDI, dec ECX
    ; EDI will now contain the address of the byte following the null-byte
    ; (ready for next 'scasb' after finding 'null')

	; calculate string length by subtracting start address from null-byte addr
	mov eax, __MAX_SCAN_BYTES      ; put 0-byte address in EAX
	sub eax, ecx                   ; put byte-length into EAX for return
	dec eax                        ; don't include 0-byte in length

	; restore registers
	pop ecx
	pop edi

	ret


