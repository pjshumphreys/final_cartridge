; ----------------------------------------------------------------
; Helper code called from Desktop
; ----------------------------------------------------------------
; Desktop doesn't know about drives or printers, so it calls into
; this library code using cross-bank calls. It also calls this to
; start a program in BASIC mode.

.include "kernal.i"
.include "persistent.i"

; from basic
.import pow10lo
.import pow10hi
.import send_drive_command
.import print_msg
.import messages
.import a_ready

; from init
.import init_basic_vectors
.import init_load_save_vectors

; fom drive
.import cmd_channel_listen
.import command_channel_talk
.import listen_second
.import talk_second

; from format
.import init_read_disk_name
.import unlisten_e2

; from printer
.import set_io_vectors
.import set_io_vectors_with_hidden_rom

.global load_and_run_program
.global perform_operation_for_desktop

.segment "desktop_helper"

reset_load_and_run:
        sei
        lda     #<$EA31
        sta     $0314
        lda     #>$EA31
        sta     $0315
        jsr     init_load_save_vectors
        jsr     init_basic_vectors
        cli
        jsr     $E3BF ; init BASIC, print banner
        jmp     _print_banner_load_and_run

; file name at $0200
load_and_run_program:
        ldx     #<(a_ready - messages) ; ("<" necessary as a compiler hint)
        jsr     print_msg ; print "READY."
        ldx     #$FB
        txs
        lda     #$80
        sta     $9D ; direct mode
        ldy     #$FF
        sty     $3A ; direct mode
        iny
        sty     $0A
        sty     FNADR
        sty     $02A8
        lda     #1 ; secondary address
        sta     SA
        lda     #>$0200
        sta     FNADR + 1 ; read filename from $0200
        sta     TXTPTR + 1
L9533:  lda     (FNADR),y
        sta     $C000,y
        beq     L953D
        iny
        bne     L9533
L953D:  sty     $B7
        lda     #$C0
        sta     FNADR + 1 ; file name pointer high (fn at $C000)
        lda     #'R'
        sta     KEYD
        lda     #'U'
        sta     KEYD + 1
        lda     #'N'
        sta     KEYD + 2
        lda     #$0D ; CR
        sta     KEYD + 3
        lda     #4 ; number of characters in kbd buffer
        sta     NDX
        jmp     $E16F ; LOAD
