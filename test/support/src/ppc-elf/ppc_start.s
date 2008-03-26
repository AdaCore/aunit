        top_of_stack   = 0x200000
        call_id_return = 0x063
        exit_status    = 0x0

        .section ".text"
        .global _start

_start:
        addis 1,0,__bss_start@h
        ori   1,1,__bss_start@l
        addis 2,0,__end@h
        ori   2,2,__end@l
        addi  3,0,0

bssloop:
        cmp   0,0,1,2
        bge   bssdone
        stwx  3,0,1
        addi  1,1,4
        b     bssloop

bssdone:
        addis 1,0,top_of_stack@h
        ori   1,1,top_of_stack@l

entry_point:
        bl      main

.globl abort
abort:
        addi 3,0,exit_status
        addi 10,0,call_id_return
        sc
