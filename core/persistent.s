; ----------------------------------------------------------------
; I/O Area ROM
; ----------------------------------------------------------------
; This is a max. 512 byte section that lives at $1E00-$1FFF of
; bank 0 of the ROM, and will also be mapped into the I/O extension
; area at $DE00-$DFFF, so it's always visible.
; It mostly contains wrappers around BASIC, KERNAL or cartridge
; functions that switch the ROM config in addition.

.include "kernal.i"

; from printer
.import new_clrch
.import new_clall
.import new_bsout
.import new_ckout

; from basic
.import reset_warmstart
.import new_tokenize
.import new_execute
.import new_expression
.import new_detokenize
.import new_mainloop

; from editor
.import kbd_handler

; from wrapper
.import disable_rom_then_warm_start

; from speeder
.import new_save
.import new_load

; from desktop_helper
.import load_and_run_program

.segment        "romio"

LDE00:  .byte   $40

.global _jmp_bank
_jmp_bank:
        sta     $DFFF
        rts

.global _enable_rom
_enable_rom: ; $DE05
        pha
        lda     #$40 ; bank 0
LDE08:  sta     $DFFF
        pla
        rts

.global _disable_rom_set_01
_disable_rom_set_01:; $DE0D
        sty     $01
.global _disable_rom
_disable_rom: ; $DE0F
        pha
        lda     #$70 ; no ROM at $8000; BASIC at $A000
        bne     LDE08

enable_all_roms:
        ora     #$07
        sta     $01
        bne     _enable_rom

.global _new_load
_new_load: ; $DE20
        tay
        lda     $01
        pha
        jsr     enable_all_roms
        jsr     new_load
LDE2B:  tax
        pla
        sta     $01
        txa
        ldx     $AE
        jmp     _disable_rom

.global _new_save
_new_save: ; $DE35
        lda     $01
        pha
        jsr     enable_all_roms
        jsr     new_save
        jmp     LDE2B

.global _new_mainloop
_new_mainloop: ; $DE41
        lda     $01
        jsr     enable_all_roms
        jmp     new_mainloop

.global _new_detokenize
_new_detokenize: ; $DE49
        jsr     _enable_rom
        jmp     new_detokenize

.global _new_expression
_new_expression: ; $DE4F
        jsr     _enable_rom
        jmp     new_expression

.global _kbd_handler
_kbd_handler:
        lda     $02A7
        beq     LDE5D
        jmp     $EB42 ; LDA #$7F : STA $DC00 : RTS

LDE5D:  lda     $A000
        cmp     #$94 ; contents of $A000 in BASIC ROM
        bne     LDF8A ; BASIC ROM not visible
        jsr     _enable_rom
        jmp     kbd_handler

LDF8A:  jmp     $EB48 ; default kdb vector

.global _load_ac_indy
_load_ac_indy: ; $DE63
        sta     $01
        lda     ($AC),y
        inc     $01
        inc     $01
        rts

.global _load_FNADR_indy
_load_FNADR_indy: ; $DE6C
        dec     $01
        lda     (FNADR),y
        inc     $01
        rts

.global _new_execute
_new_execute: ; $DE73
        jsr     _CHRGET
        jsr     new_execute
        jsr     _disable_rom
        jmp     $A7AE ; CLEAR

.global _new_warmstart
_new_warmstart:
        jsr     _enable_rom
        jsr     reset_warmstart
        jmp     disable_rom_then_warm_start

.global _ay_to_float
_ay_to_float: ; $DEF0
        jsr     _disable_rom
        jsr     $B395 ; convert A/Y to float
        jmp     LDEFF

.global _int_to_fac
_int_to_fac: ; $DEF9
        jsr     _disable_rom
        jsr     $BBA6 ; convert $22/$23 to FAC
LDEFF:  iny
        jsr     $BDD7 ; print FAC
        jmp     _enable_rom

.global _search_for_line
_search_for_line: ; $DF0F
        jsr     _disable_rom
        jsr     $A613 ; search for BASIC line
        jmp     LDF21

.global _CHRGET
_CHRGET: ; $DF1B
        jsr     _disable_rom
        jsr     CHRGET
LDF21:  php
        jsr     _enable_rom
        plp
        rts

.global _CHRGOT
_CHRGOT: ; $DF27
        jsr     _disable_rom
        jsr     CHRGOT
        jmp     LDF21

.global _new_tokenize
_new_tokenize: ; $DF8D
        jsr     _enable_rom
        jsr     new_tokenize
        jmp     _disable_rom

; calls into banks 0+1
.global _new_ckout
_new_ckout: ; $DFC0
        jsr     _enable_rom
        jsr     new_ckout
        jmp     _disable_rom

.global _new_bsout
_new_bsout: ; $DFC9
        jsr     _enable_rom
        jmp     new_bsout

.global _new_clall
_new_clall: ; $DFCF
        jsr     _enable_rom
        jmp     new_clall

.global _new_clrch
_new_clrch: ; $DFD5
        jsr     _enable_rom
        jmp     new_clrch

.global LDFE0
LDFE0: ; XXX BUG ???
.global _bar_irq
_bar_irq: ; XXX BUG ???
        lda     #$40 ; bank 2 (Desktop, Freezer/Print)
        sta     $DFFF
        jmp     $FCE2

.global _a_colon_asterisk
_a_colon_asterisk:
        .byte   ':','*'
.global _a_colon_asterisk_end
_a_colon_asterisk_end:

; ----------------------------------------------------------------
; I/O Area ROM End
; ----------------------------------------------------------------
