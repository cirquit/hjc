.intel_syntax
.globl Lmain

Lmain:
    push %ebp
    mov %ebp, %esp
    
    push 23
    call L_println_int
    add %esp, 4
    mov %esp, %ebp
    pop %ebp
    ret
