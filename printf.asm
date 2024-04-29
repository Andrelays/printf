;========================================================================================
;Convention Function
;RSP, RBP, RBX, R12, R13, R14, R15 - nonvalotile save in stack by called function
;RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, - valotile save by code which calls
;ABI GCC:       ;r9 - first 6 arguments in registers, than stack
                ;r8
                ;rcx
                ;rdx
                ;rsi
                ;rdi <----- first arg
                ;rax - return reg
;Convention Syscall:
                ;rcx
                ;rdi - args for syscall
                ;rsi
                ;rdx
                ;r11
                ;r10
                ;r8
                ;r9
;========================================================================================

SIZE_BUFFER     equ 10
FAIL_RET        equ 0
STACK_OFFSET    equ 8
STRING_OFFSET   equ STACK_OFFSET * 8

global my_printf

section .text
;========================================MAIN=FUNCTION===================================
;my_printf(char* buffer, ...) - function supporting because of ABI
;========================================================================================

my_printf:
    pop  r10        ;pop in r10 address of call

    push r9         ;save all regs in stack
    push r8         ;now i have all args in stack
    push rcx
    push rdx
    push rsi
    push rdi

    push rbp        ;save regs by convention
    push rbx
    push r12
    push r13
    push r14
    push r15

    call print_func

    pop r15         ;revive regs
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp

    pop rdi         ;revive all args
    pop rsi
    pop rdx
    pop rcx
    pop r8
    pop r9

    push r10        ;put in stack address of return from Printf32

    ret
;==================================PRINT_FUNC============================================
;printf_func(char* buffer, ...) - function takes arguments from the stack and prints a string
;Args: arguments in stack
;Ret: RAX
;Change: rbx, rdi, rcx, r11, r12, r13, r14, r15
;========================================================================================

print_func:
    push rbp                                ;save rbp
    mov rbp, rsp                            ;new rbp = rsp

    xor r15, r15                            ;r15 - counter of output characters
    xor r11, r11                            ;r11 - character counter in the buffer
    mov rbx, qword [rbp + STRING_OFFSET]    ;rbx - pointer to the buffer
    mov r14, rbp
    add r14, STACK_OFFSET + STRING_OFFSET   ;r14 - the following argument

.while_start:
    cmp byte [rbx], 0                       ;char == '\0' (the end of the buffer)
    je .while_end

.if_start:
    cmp byte [rbx], '%'
    jne .if_else

    inc rbx                                 ;set [rbx] to the next character after '%'

    cmp byte [rbx], 0                       ;char == '\0' (check the last symbol is % - ERROR!)
    je .error_return

    cmp byte [rbx], '%'                     ;1) %%
    je .if_else

    cmp byte [rbx], 'n'                     ;2)%n
    je .assigns_variable

    xor r13, r13                            ;3) %?
    mov r13b, byte [rbx]                    ;r13 = byte [rbx]

    jmp [jump_table_of_printf + (r13 - 'b') * 8]

.assigns_variable:
    push r13                                ;save r13
    mov r13, [r14]
    mov [r13], r15d
    add [r13], r11d                         ;&arg = counter of output characters + character counter in the buffer
    pop r13                                 ;ret r13

.return_from_table:
    add r14, STACK_OFFSET
    jmp .if_end

.if_else:
    mov r12b, byte [rbx]                    ;save char to write
    call save_char

.if_end:
    inc rbx
    jmp .while_start

.while_end:
    mov rax, r15
    add rax, r11                            ;successful completion of the function
    call write_buffer
    jmp .end_of_func

.error_return:
    mov rax, FAIL_RET                       ;erroneous completion of the function
    call write_buffer

.end_of_func:
    pop rbp

    ret

;==================================WRITE_BUFFER==========================================
;write_buff() - function of write buffer in stdout
;Args: r11 - character counter in the buffer
;Ret: nothing
;Change: nothing
;========================================================================================

write_buffer:
    push rax
    push rdi
    push rsi                ;save regs
    push rdx
    push rcx                ;syscall may change rcx and r11
    push r11

    mov rax, 1              ;syscall - write
    mov rdi, 1              ;stdout
    mov rsi, buffer_out     ;address of str to write
    mov rdx, r11            ;length

    syscall

    pop r11
    pop rcx
    pop rdx                 ;ret regs
    pop rsi
    pop rdi
    pop rax

    ret

;==================================SAVE_CHAR=============================================
;save_char() - function saves one character to the buffer
;Args: r11 - character counter in the buffer, r12b - the symbol that needs to be saved
;Ret: nothing
;Change: r15, r11, buffer_out
;========================================================================================

save_char:

.if_start:
    cmp r11, SIZE_BUFFER
    jb .if_end

    call write_buffer                   ;if the buffer is full, output it and reset it

    push rcx                            ;save rcx
    add r15, r11                        ;increasing the counter of output characters by r11
    mov rcx, r11

.for_start:
    mov buffer_out[rcx], byte 0         ;resetting the buffer
    loop .for_start

    pop rcx
    mov r11, 0

.if_end:

    mov buffer_out[r11], r12b           ;save char from r12b
    inc r11

    ret

;==================================PRINT_BIN==============================================
;print_bin() - function print binary representation of a number from arg of printf
;Args: r11 - character counter in the buffer, byte [r14] - the number that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_bin:
    push r8
    push rcx
    push rdx

    mov r12b, '0'                   ;print 0b - prefix of bin
    call save_char
    mov r12b, 'b'
    call save_char

    mov r8, [r14]                   ;r8 - number from arg
    xor rcx, rcx                    ;rcx - counter of digit

.while_start:
    inc rcx

    mov rdx, r8                     ;rdx - digit
    and rdx, 1                      ;rdx %= 2
    push rdx                        ;save digit in stack

    shr r8, 1                       ;r8 /= 2
    test r8, r8
    je .while_end

    jmp .while_start
.while_end:

.for_start:
    pop r12
    add r12b, '0'
    call save_char                  ;print number from stack
    loop .for_start

    pop rdx
    pop rcx
    pop r8

    jmp print_func.return_from_table

;==================================PRINT_CHAR=============================================
;print_char() - function print one character from arg of printf
;Args: r11 - character counter in the buffer, byte [r14] - the symbol that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_ch:
    mov r12b, byte [r14]            ;save char to write
    call save_char
    jmp print_func.return_from_table


;==================================PRINT_DEC_SIGN=========================================
;print_dec_sign() - function print decimal signed representation of a number from arg of printf
;Args: r11 - character counter in the buffer, [r14] - the number that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_dec_sign:
    push r8
    push rcx                        ;save regs
    push rdx
    push rax

    xor rcx, rcx                    ;rcx - counter of digit

    mov rax, [r14]                  ;r8 - number from arg
    mov r8, 10
    cmp eax, 0
    jg .while_start

    neg eax
    mov r12, '-'
    call save_char
    
.while_start:
    inc rcx
    cqo
    div r8

    push rdx                        ;save digit in stack

    test rax, rax
    je .while_end

    jmp .while_start
.while_end:

.for_start:
    pop r12
    mov r12, hex_str[r12]
    call save_char                  ;print number from stack
    loop .for_start

    pop rax
    pop rdx
    pop rcx
    pop r8

    jmp print_func.return_from_table

;==================================PRINT_OCT==============================================
;print_oct() - function print octal representation of a number from arg of printf
;Args: r11 - character counter in the buffer, byte [r14] - the number that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_oct:
    push r8
    push rcx
    push rdx

    mov r12b, '0'                   ;print 0o - prefix of oct
    call save_char
    mov r12b, 'o'
    call save_char

    mov r8, [r14]                   ;r8 - number from arg
    xor rcx, rcx                    ;rcx - counter of digit

.while_start:
    inc rcx

    mov rdx, r8                     ;rdx - digit
    and rdx, 7                      ;rdx %= 8
    push rdx                        ;save digit in stack

    shr r8, 3                       ;r8 /= 8
    test r8, r8
    je .while_end

    jmp .while_start
.while_end:

.for_start:
    pop r12
    add r12b, '0'
    call save_char                  ;print number from stack
    loop .for_start

    pop rdx
    pop rcx
    pop r8

    jmp print_func.return_from_table

;==================================PRINT_STR=============================================
;print_str() - a function that printed a string from an address using the printf argument
;Args: r11 - character counter in the buffer, [r14] - the address of the string to be printed
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_str:
    push r8                         ;save r8
    mov r8, qword [r14]             ;r8 - pointer to the string

.while_start:
    cmp byte [r8], 0
    je .while_end

    mov r12b, byte [r8]
    call save_char

    inc r8
    jmp .while_start
.while_end:

    pop r8                          ;ret r8

    jmp print_func.return_from_table

;==================================PRINT_DEC_UNSIGN======================================
;print_dec_unsign() - function print decimal unsigned representation of a number from arg of printf
;Args: r11 - character counter in the buffer, [r14] - the number that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_dec_unsign:
    push r8
    push rcx                        ;save regs
    push rdx
    push rax

    mov rax, [r14]                  ;r8 - number from arg
    mov r8, 10
    xor rcx, rcx                    ;rcx - counter of digit

.while_start:
    inc rcx
    cqo
    div r8

    push rdx                        ;save digit in stack

    test rax, rax
    je .while_end

    jmp .while_start
.while_end:

.for_start:
    pop r12
    mov r12, hex_str[r12]
    call save_char                  ;print number from stack
    loop .for_start

    pop rax
    pop rdx
    pop rcx
    pop r8

    jmp print_func.return_from_table

;==================================PRINT_HEX==============================================
;print_hex() - function print hexadecimal representation of a number from arg of printf
;Args: r11 - character counter in the buffer, byte [r14] - the number that needs to be saved
;Ret: nothing
;Change: r12, r15, r11, buffer_out
;=========================================================================================

print_hex:
    push r8
    push rcx
    push rdx

    mov r12b, '0'                   ;print 0x - prefix of hex
    call save_char
    mov r12b, 'x'
    call save_char

    mov r8, [r14]                   ;r8 - number from arg
    xor rcx, rcx                    ;rcx - counter of digit

.while_start:
    inc rcx

    mov rdx, r8                     ;rdx - digit
    and rdx, 15                     ;rdx %= 16
    push rdx                        ;save digit in stack

    shr r8, 4                       ;r8 /= 16
    test r8, r8
    je .while_end

    jmp .while_start
.while_end:

.for_start:
    pop r12
    mov r12, hex_str[r12]
    call save_char                  ;print number from stack
    loop .for_start

    pop rdx
    pop rcx
    pop r8

    jmp print_func.return_from_table

;========================================================================================

section .rodata

hex_str: db '0123456789ABCDEF'

error_buffer:          ;Extra buffer before jmp table if user write %?
        times 'b'+1     dq      print_func.error_return  ;error buffer before '\0'-'b'

jump_table_of_printf:
                        dq      print_bin
                        dq      print_ch
                        dq      print_dec_sign
        times 'o'-'d'-1 dq      print_func.error_return
                        dq      print_oct
                        dq      print_hex
        times 's'-'p'-1 dq      print_func.error_return
                        dq      print_str
        times 'u'-'s'-1 dq      print_func.error_return
                        dq      print_dec_unsign
        times 'x'-'u'-1 dq      print_func.error_return
                        dq      print_hex
        times '~'-'x'   dq      print_func.error_return  ;error buffer after '~'-'x'

section .data
;buffer for print
buffer_out:     times SIZE_BUFFER db '0'
