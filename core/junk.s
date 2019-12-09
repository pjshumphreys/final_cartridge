; ----------------------------------------------------------------
; junk - this is a part of "new_load2"
.include "kernal.i"

.import _enable_rom
.import _disable_rom

.segment "junk"

move_and_run_code:
    php ;backup flags
    sta $02AD ;backup a register
    stx $02AE ;backup x register

    ;create a code trampoline
    lda #$BD; lda $xxxx,X
    sta $02AF

    ;load the address at the stack pointer into our trampoline
    tsx
    inx
    inx
    lda $0100, X
    sta $02B0
    inx
    lda $0100, X
    sta $02B1

    lda #$60; rts
    sta $02B2

    ;increment the address
    inc $02B0
    bne skiphi
    inc $02B1

    ;use the trampoline to copy jump code from rom to ram
skiphi:
    ldx #$12
memcpy:
    dex
    jsr $02AF
    inx
    sta $02B2, X    ; 2 bytes, 6 cycles
    dex             ; 1 byte, 2 cycles
    bne memcpy      ; 2 bytes, 2-3 cycles

    ;put jsr _disable_rom at the start
    lda #$20; jsr
    sta $02B0
    lda #<_disable_rom
    sta $02B1
    lda #>_disable_rom
    sta $02B2

    pla ;temporarily restore flags to register a

    ;remove the old return address from the stack
    tsx
    inx
    inx
    txs

    pha
    ldx $02AE ;restore register x value
    lda $02AD ; restore register a value
    plp ;restore flags
    jmp $02B0 ; jump to the code that's now in ram

.global _execute_statement
_execute_statement: ; $DE7F
        jsr     move_and_run_code
        jmp     $A7EF ; execute BASIC statement

.global _basic_warm_start
_basic_warm_start: ; $DE14
        jsr     move_and_run_code
        jmp     $E37B ; BASIC warm start (NMI)

.global _add_A_to_FAC
_add_A_to_FAC: ; $DE85
        jsr     move_and_run_code
        jsr     $BD7E ; add A to FAC
        jmp     _enable_rom

.global _expression_cont
_expression_cont: ; $DE8E
        jsr     move_and_run_code
        jmp     $AE8D ; get element in expression

.global _evaluate_modifier
_evaluate_modifier: ; $DEA9
        jsr     move_and_run_code
        jmp     $EB48 ; evaluate SHIFT/CTRL/C=

.global _get_line_number
_get_line_number: ; $DEAF
        jsr     move_and_run_code
        jsr     $A96B ; get line number
        jmp     _enable_rom

.global _basic_bsout
_basic_bsout: ; $DEB8
        jsr     move_and_run_code
        jsr     $AB47 ; print character
        jmp     _enable_rom

.global _set_txtptr_to_start
_set_txtptr_to_start: ; $DEC1
        jsr     move_and_run_code
        jsr     $A68E ; set TXTPTR to start of program
        jmp     _enable_rom

.global _check_for_stop
_check_for_stop: ; $DECA
        jsr     move_and_run_code
        jsr     $A82C ; check for STOP
        jmp     _enable_rom

.global _relink
_relink: ; $DED3
        jsr     move_and_run_code
        jsr     $A533 ; rebuild BASIC line chaining
        jmp     _enable_rom

.global _get_filename
_get_filename: ; $DEDB
        jsr     move_and_run_code
        jsr     $E257 ; get string from BASIC line, set filename
        jmp     _enable_rom

.global _lda_5a_indy
_lda_5a_indy: ; $DF30
        jsr     move_and_run_code
        lda     ($5A),y
        jmp     _enable_rom

.global _lda_5f_indy
_lda_5f_indy: ; $DF38
        jsr     move_and_run_code
        lda     ($5F),y
        jmp     _enable_rom

.global _sbc_5f_indy
_sbc_5f_indy: ; $DF38
        jsr     move_and_run_code
        sbc     ($5F),y
        jmp     _enable_rom

.global _lda_ae_indx
_lda_ae_indx: ; $DF40
        jsr     move_and_run_code
        lda     ($AE,x)
        jmp     _enable_rom

.global _lda_TXTPTR_indy
_lda_TXTPTR_indy: ; $DF48
        jsr     move_and_run_code
        lda     (TXTPTR),y
        jmp     _enable_rom

.global _lda_TXTPTR_indx
_lda_TXTPTR_indx: ; DF50
        jsr     move_and_run_code
        lda     (TXTPTR,x)
        jmp     _enable_rom

.global _lda_22_indy
_lda_22_indy: ; $DF58
        jsr     move_and_run_code
        lda     ($22),y
        jmp     _enable_rom

.global _lda_8b_indy
_lda_8b_indy: ; $DF60
        jsr     move_and_run_code
        lda     ($8B),y
        jmp     _enable_rom

_detokenize: ; $DF68
        jsr     move_and_run_code
        jmp     $A724 ; detokenize

.global _list
_list: ; $DF6E
        jsr     move_and_run_code
        jmp     $A6F3 ; part of LIST

.global _print_ax_int
_print_ax_int: ; $DF06
        jsr     move_and_run_code
        jsr     $BDCD ; LINPRT print A/X as integer
        jmp     _enable_rom

.global _print_banner_load_and_run
.import load_and_run_program
_print_banner_load_and_run: ; $DF74
        jsr     move_and_run_code
        jsr     $E422 ; print c64 banner
        jsr     _enable_rom
        jmp     load_and_run_program

.global _get_int
_get_int: ; $DE94
        jsr     move_and_run_code
        jsr     $AD8A ; FRMNUM eval expression, make sure it's numeric
        jsr     $B7F7 ; GETADR convert FAC into 16 bit int
        jmp     _enable_rom

.global _int_to_ascii
_int_to_ascii: ; $DEE4
        jsr     move_and_run_code
        jsr     $BC49 ; FLOAT UNSIGNED VALUE IN FAC+1,2
        jsr     $BDDD ; convert FAC to ASCII
        jmp     _enable_rom

.global _search_for_line
_search_for_line: ; $DF0F
        jsr     move_and_run_code
        jsr     $A613 ; search for BASIC line
        php
        jsr     _enable_rom
        plp
        rts

