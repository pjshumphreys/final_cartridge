; ----------------------------------------------------------------
; Disk and Tape Speeder
; ----------------------------------------------------------------
; This speeds up LOAD and SAVE on both disk and tape

.include "kernal.i"
.include "persistent.i"

.global new_load
.global new_save

L0110           := $0110

.segment "speeder_a"
.res 10,$ff
new_load:
  jmp new_load2
new_save:
  jmp new_save2

send_byte:
        pha
loc_9907:
        bit     $DD00
        bpl     loc_9907
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        tax
loc_9911:
        lda     $D012
        cmp     #$31
        bcc     loc_991E
        and     #$06
        cmp     #$02
        beq     loc_9911
loc_991E:
        lda     #$07
        sta     $DD00
        lda     iec_tab,x
        nop
        nop
        sta     $DD00
        lsr     a
        lsr     a
        and     #$F7
        sta     $DD00
        pla
        and     #$0F
        tax
        lda     iec_tab,x
        sta     $DD00
        lsr     a
        lsr     a
        and     #$F7
        sta     $DD00
        lda     #$17
        nop
        nop
        sta     $DD00
        rts
.assert >* = >send_byte, error, "Page boundary!"

iec_tab:
        .byte   $07,$87,$27,$A7,$47,$C7,$67,$E7
        .byte   $17,$97,$37,$B7,$57,$D7,$77,$F7
.assert >* = >iec_tab, error, "Page boundary!"

receive_4_bytes:
        lda     $02A6   ; PAL or NTSC?
        beq     L998B
loc_9962:
        ; PAL
        bit     $DD00
        bvs     loc_9962
        ldy     #3
        nop
        ldx     $01
loc_996C:
        lda     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        sta     $C1,y
        dey
        bpl     loc_996C
.assert >* = >receive_4_bytes, error, "Page boundary!"
        rts

L998B:  bit     $DD00
        bvs     L998B
        ldy     #3
        nop
        ldx     $01
L9995:  ;NTSC
        lda     $DD00
        lsr     a
        lsr     a
        nop
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        lsr     a
        lsr     a
        nop
        nop
        ora     $DD00
        sta     $C1,y
        dey
        bpl     L9995
        rts
.assert >* = >L998B, error, "Page boundary!"

; *** tape
L99B5:  tax
        beq     L99C3
        ldx     #$16
loc_99BA:
        lda     L9A50,x
        sta     L0110,x
        dex
        bpl     loc_99BA
L99C3:  jmp     LA851
; *** tape

L99C6:  jmp     $F530 ; IEC LOAD - used in the error case

L99C9:  pla
        pla
        pla
        tay
        lda     #$F4
        pha
        lda     #$A6
        pha
        jmp     _disable_rom_set_01

L99D6:  pla
        pla
        pla
        tay
        lda     #$F4
        pha
        lda     #$F2
        pha
        jmp     _disable_rom_set_01

new_load2:
        sty     $93
        tya
        ldy     FA
        cpy     #7
        beq     L99B5 ; tape turbo
        cpy     #8
        bcc     L99C9
        cpy     #10
        bcs     L99C9
        tay
        lda     $B7
        beq     L99C9
        jsr     _load_FNADR_indy
        cmp     #$24
        beq     L99C9
        ldx     SA
        cpx     #2
        beq     L99C9
        jsr     print_searching
        lda     #$60
        sta     SA
        jsr     LA71B
        lda     FA
        jsr     $ED09 ; TALK
        lda     SA
        jsr     $EDC7 ; SECTLK
        jsr     $EE13 ; IECIN
        sta     $AE
        lda     ST
        lsr     a
        lsr     a
        bcs     L99C6
        jsr     $EE13 ; IECIN
        sta     $AF
        txa
        bne     L9A35
        lda     $C3
        sta     $AE
        lda     $C4
        sta     $AF
L9A35:  jsr     print_loading
        lda     $AF
        cmp     #4
        bcc     L99D6
        jmp     L9AF0

; ----------------------------------------------------------------

.segment "tape_stack_code"

; will be placed at $0100
load_ac_indy:
        lda     #$0C
        sta     $01
        lda     ($AC),y
        ldy     #$0F
        sty     $01
        ldy     #0
        jmp     LA9BB
load_ac_indy_end:

L9A50:  lda     #$0C
        sta     $01
        lda     ($C3),y
        cmp     $BD
        beq     loc_9A5C
        stx     ST
loc_9A5C:
        eor     $D7
        sta     $D7
        lda     #$0F
        sta     $01
        jmp     LA8FF

.segment "speeder_b"

L9A67:  jmp     $F636 ; LDA #0 : SEC : RTS

L9A6A:  jmp     $F5ED ; default SAVE vector

L9A6D:  jmp     loc_A7C6 ; interpreter loop

new_save2:
        lda     FA
        cmp     #7
        beq     L9A6D ; tape turbo
        cmp     #8
        bcc     L9A6A ; not a drive
        cmp     #10
        bcs     L9A6A ; not a drive (XXX why only support drives 8 and 9?)
        ldy     $B7
        beq     L9A6A
        lda     #$61
        sta     SA
        jsr     LA71B
        jsr     LA77E
        jsr     LA648
        bne     L9A67
        stx     ST
        stx     $A4
        jsr     $FB8E ; copy I/O start address to buffer address
        sec
        lda     $AC
        sbc     #2
        sta     $AC
        bcs     L9AA3
        dec     $AD
L9AA3:  jsr     L9AD0
        lda     $C1
        jsr     L9AC7
        lda     $C2
        jsr     L9AC7
L9AB0:  lda     #$35
        jsr     _load_ac_indy
        jsr     L9AC7
        bne     L9AB0
        lda     $A4
        bmi     L9AC4
        jsr     L9AD0
        jmp     L9AB0

L9AC4:  cli
        clc
        rts

L9AC7:  jsr     send_byte
        jsr     $FCDB ; inc $AC/$AD
        dec     $93
        rts

L9AD0:  sec
        lda     $AE
        sbc     $AC
        tax
        sta     $93
        lda     $AF
        sbc     $AD
        bne     L9AE8
        cpx     #$FF
        beq     L9AE8
        inx
        txa
        dec     $A4
        bne     L9AED
L9AE8:  lda     #$FE
        sta     $93
        tya
L9AED:  jmp     send_byte

L9AF0:  jsr     UNTALK
        jsr     LA691
        lda     #6
        sta     $93
.import __drive_code_load_LOAD__
.import __drive_code_load_RUN__
        lda     #<__drive_code_load_LOAD__
        ldy     #>__drive_code_load_LOAD__
        ldx     #>__drive_code_load_RUN__ ; $0400
        jsr     transfer_code_to_drive
        lda     #$9A
        jsr     IECOUT
        lda     #$5
        jsr     IECOUT
        jsr     UNLSTN
        sei
        lda     $D011
        tax
        and     #$10 ; save screen enable bit
        sta     $95
        txa
        and     #$EF
        sta     $D011
        lda     $DD00
        and     #$07
        ora     $95 ; save VIC bank (XXX #$03 would have been enough)
        sta     $95
        lda     $C1
        sta     $A4
        lda     $C2
        sta     SA
        sec
        lda     $AE
        sbc     #2
        sta     ST
        lda     $AF
        sbc     #0
        sta     $A3
L9B3D:  bit     $DD00
        bmi     L9B82
        cli
        php
        lda     $95
        and     #$07
        sta     $DD00 ; restore VIC bank
        lda     $95
        and     #$10
        ora     $D011 ; restore screen enable bit
        sta     $D011
        lda     $A4
        sta     $C1
        lda     SA
        sta     $C2
        lda     #0
        sta     $A3
        sta     $94
        sta     ST
        lda     #$60
        sta     SA
        lda     #$E0
        jsr     LA612
        jsr     UNLSTN
        plp
        bvs     L9B78 ; used to be "bcs" in 1988-05
        lda     #$1D
        sec
        rts

L9B78:  lda     #$40
        sta     ST
        jsr     LA694
        jmp     $F5A9 ; LOAD done

L9B82:  bvs     L9B3D
        lda     #$20
        sta     $DD00
loc_9B89:
        bit     $DD00
        bvc     loc_9B89
        lda     #0
        sta     $DD00
        jsr     receive_4_bytes
        lda     #$FE
        sta     $A5
        lda     $C3
        clc
        adc     $A3
        tax
        asl     $C3
        php
        sec
        lda     ST
        sbc     $C3
        sta     $93
        bcs     L9BAD
        dex
L9BAD:  plp
        bcc     L9BB1
        dex
L9BB1:  stx     $94
        ror     $C3
        ldx     $C2
        beq     L9BC8
        dex
        stx     $A5
        txa
        clc
        adc     $93
        sta     $AE
        lda     $94
        adc     #0
        sta     $AF
L9BC8:  ldy     #0
        lda     $C3
        bne     L9BD7
        jsr     receive_4_bytes
        ldy     #2
        ldx     #2
        bne     L9BE5
L9BD7:  lda     $C1
        sta     ($93),y
        iny
L9BDC:  tya
        pha
        jsr     receive_4_bytes
        pla
        tay
        ldx     #3
L9BE5:  cpy     $A5
        bcs     L9BED
        lda     $C1,x
        sta     ($93),y
L9BED:  iny
        cpy     #$FE
        bcs     L9BF7
        dex
        bpl     L9BE5
        bmi     L9BDC
L9BF7:  jmp     L9B3D

; ----------------------------------------------------------------
                A1L      :=     $5A;$2A;$3C        ;MOVE source start
                A1H      :=     $5B;$3D
                A2L      :=     $FB;$3E        ;MOVE source end
                A2H      :=     $FC;$3F
                A4L      :=     $FD;$42        ;MOVE dest start
                A4H      :=     $FE;$43

                STREND   :=     $31 ; $6D        ;Ptr to end of vars
                LOWTR    :=     $5F ; $9B        ;GETARYPT puts address here
                ;CHRGET   :=     $73 ; $B1        ;Get next token
                ; doesn't exist in c64. build it outselves  11   GETARYPT =     $F7D9      ;Find array in memory
                ; doesn't exist in c64. build it outselves 12   MOVE     =     $FE2C      ;Block move
                ARYTAB := $2F
                VARNAM := $45
                ERROR := disable_rom_jmp_error ;$A437
                ERR_NODATA := $0D

ISLETC:
  CMP #$41  ; "A"
  BCC ISLRTS          ;IF LESS THAN "A", RET.
  SBC #$5B  ; "Z"+1
  SEC
  SBC #$A5  ; 256-"Z"-1       ;RESET CARRY IF [A] .GT. "Z".
ISLRTS:
  RTS                     ;RETURN TO CALLER.

.import _lda_5f_indy
.import _sbc_5f_indy
.global ERASE
ERASE:
  STA VARNAM
  JSR _CHRGOT
  JSR ISLETC
  BCS LB09F
LB09C:
  LDX #$0B
  JMP disable_rom_jmp_error

LB09F:
  LDX #$00
  STX $0D
  STX $0E
  JSR _CHRGET
  BCC LB0AF
  JSR ISLETC
  BCC LB0BA
LB0AF:
  TAX
LB0B0:
  JSR _CHRGET
  BCC LB0B0
  JSR ISLETC
  BCS LB0B0
LB0BA:
  CMP #$24   ; $
  BNE LB0C4
  LDA #$FF
  STA $0D
  BNE LB0D4
LB0C4:
  CMP #$25   ; %
  BNE LB0DB
  LDA $10
  BNE LB09C
  LDA #$80
  STA $0E
  ORA VARNAM
  STA VARNAM
LB0D4:
  TXA
  ORA #$80
  TAX
  JSR _CHRGET
LB0DB:
  STX VARNAM+1

GETARYPT:
    ldx     ARYTAB          ;(A,X) = start of array table
    lda     ARYTAB+1
LE16D:
    stx     LOWTR           ;use LOWTR for running pointer
    sta     LOWTR+1
    cmp     STREND+1        ;did we reach the end of arrays yet?
    bne     LE179           ;no, keep searching
    cpx     STREND
    beq     MAKE_NEW_ARRAY  ;yes, this is a new array name
LE179:
    ldy     #$00            ;point at 1st char of array name
    jsr     _lda_5f_indy    ;lda     (LOWTR),y       ;get 1st char of name
    iny                     ;point at 2nd char
    cmp     VARNAM          ;1st char same?
    bne     LE188           ;no, move to next array
    jsr     _lda_5f_indy
    sta     $02
    lda     VARNAM+1        ;yes, try 2nd char
    cmp     $02             ;same?
    beq     USE_OLD_ARRAY   ;yes, array found
LE188:
    ldy     #$02            ;point at offset to next array
    jsr     _lda_5f_indy    ;lda     (LOWTR),y       ;add offset to running pointer
    clc
    adc     LOWTR
    tax
    iny
    jsr     _lda_5f_indy    ;lda     (LOWTR),y
    adc     LOWTR+1
    bcc     LE16D           ;...always

.import disable_rom_jmp_error
MAKE_NEW_ARRAY:
    ldx     #ERR_NODATA     ;yes, give "out of data" error
    jmp     disable_rom_jmp_error

USE_OLD_ARRAY:
    ;jmp LB24D
    sec                     ;signal array found
;    rts

    LDY   #2         ;Offset to array len
    LDA   LOWTR
    STA   A4L
    CLC
    jsr   _lda_5f_indy    ;LDA   (LOWTR),Y  ;Next array address -> MOVE src start
    ADC   LOWTR
    STA   A1L
    LDA   LOWTR+1
    STA   A4H
    INY
    php
    jsr   _lda_5f_indy    ;LDA   (LOWTR),Y
    plp
    ADC   LOWTR+1
    STA   A1H
    SEC
    LDA   STREND     ;ALSO CORRECT THE END
    STA   A2L        ;OF VARIABLES POINTER
    DEY
    jsr   _sbc_5f_indy
    STA   STREND
    LDA   STREND+1
    STA   A2H
    INY
    jsr   _sbc_5f_indy    ;LDA   (LOWTR),Y
    STA   STREND+1
    LDY   #0
;    JMP   MOVE       ;GO MOVE AND RETURN

MOVE:
    ;LDA (A1L),Y ; MOVE (A1 TO A2) TO
    .global _lda_5a_indy
    jsr _lda_5a_indy
    STA (A4L),Y ; (A4)
;    JSR NXTA4 ;(fcb4)

NXTA4:
    INC A4L ;INCR 2-BYTE A4
    BNE NXTA1 ;AND A1
    INC A4H
NXTA1:
    LDA A1L ;INCR 2-BYTE A1.
    CMP A2L
    LDA A1H ;AND COMPARE TO A2
    SBC A2H
    INC A1L ;(CARRY SET IF &gt=)
    BNE RTS4B
    INC A1H
RTS4B:
;    RTS


    BCC MOVE
    RTS


.segment "drive_code_load" ; $0400

drive_code_load:
.incbin "drive_load.bin"

; ----------------------------------------------------------------
; drive code $0500
; ----------------------------------------------------------------
.segment "drive_code_save"

drive_code_save:
.incbin "drive_save.bin"

; ----------------------------------------------------------------
; C64 IEC code
; ----------------------------------------------------------------
.segment "speeder_c"

LA612:  pha
        lda     FA
        jsr     LISTEN
        pla
        jmp     SECOND

LA61C:  lda     #$6F
        pha
        lda     FA
        jsr     TALK
        pla
        jmp     TKSA

LA628:  jsr     LA632
        jsr     $E716 ; KERNAL: output character to screen
        tya
        jmp     $E716 ; KERNAL: output character to screen

LA632:  pha
        and     #$0F
        jsr     LA63E
        tay
        pla
        lsr     a
        lsr     a
        lsr     a
        lsr     a
LA63E:  clc
        adc     #$F6
        bcc     LA645
        adc     #$06
LA645:  adc     #$3A
LA647:  rts

LA648:
        jsr     LA6C1
        bne     LA647
        lda     #7
        sta     $93
.import __drive_code_save_LOAD__
.import __drive_code_save_RUN__
        lda     #<__drive_code_save_LOAD__
        ldy     #>__drive_code_save_LOAD__
        ldx     #>__drive_code_save_RUN__
        jsr     transfer_code_to_drive
        lda     $0330
        cmp     #<_new_load
        beq     LA66A ; speeder enabled
        lda     #$9C
        jsr     IECOUT
        lda     #$05
        bne     LA671

LA66A:  lda     #$AF
        jsr     IECOUT
        lda     #$05
LA671:  jsr     IECOUT
        jsr     UNLSTN
        sei
        lda     $D015
        sta     $93
        sty     $D015
        lda     $DD00
        and     #$07
        sta     $A4
        ora     #$10
        sta     $A5
        sta     $DD00
        jmp     LA9F6

LA691:
        ldy     #0
        .byte   $2C
LA694:
        ldy     #8
        bit     $9D
        bpl     LA6A7
        jsr     LA6A8
        lda     $AF
        jsr     LA628
        lda     $AE
        jmp     LA628

LA6A7:  rts

LA6A8:  lda     s_from,y
        beq     LA6A7
        jsr     $E716 ; KERNAL: output character to screen
        iny
        bne     LA6A8

s_from: .byte   " FROM $", 0
        .byte   " TO $", 0

LA6C1:  jsr     LA61C
        jsr     IECIN ; first character, ASCII error code
        tay
LA6C8:  jsr     IECIN
        cmp     #CR
        bne     LA6C8 ; read until CR
        jsr     UNTALK
        cpy     #'0' ; = no error
        rts

transfer_code_to_drive:
        sta     $C3
        sty     $C4
        ldy     #0
LA6DB:  lda     #'W'
        jsr     LA707 ; send "M-W"
        tya
        jsr     IECOUT
        txa
        jsr     IECOUT
        lda     #$20
        jsr     IECOUT
LA6ED:  lda     ($C3),y
        jsr     IECOUT
        iny
        tya
        and     #$1F
        bne     LA6ED
        jsr     UNLSTN
        tya
        bne     LA6DB
        inc     $C4
        inx
        cpx     $93
        bcc     LA6DB
        lda     #'E' ; send "M-E"
LA707:  pha
        lda     #$6F
        jsr     LA612
        lda     #'M'
        jsr     IECOUT
        lda     #'-'
        jsr     IECOUT
        pla
        jmp     IECOUT

LA71B:
        ldy     #0
        sty     ST
        lda     FA
        jsr     $ED0C ; LISTEN
        lda     SA
        ora     #$F0
        jsr     $EDB9 ; SECLST
        lda     ST
        bpl     LA734
        pla
        pla
        jmp     $F707 ; DEVICE NOT PRESENT ERROR

LA734:  jsr     _load_FNADR_indy
        jsr     $EDDD ; KERNAL IECOUT
        iny
        cpy     $B7
        bne     LA734
        jmp     $F654 ; UNLISTEN

LA742:  jsr     $F82E ; cassette sense
        beq     LA764
        ldy     #$1B
LA749:  jsr     LA7B3
LA74C:  bit     $DC01
        bpl     LA766
        jsr     $F82E ; cassette sense
        bne     LA74C
        ldy     #$6A
        jmp     LA7B3

LA75B:  jsr     $F82E ; cassette sense
        beq     LA764
        ldy     #$2E
LA762: ; ???
        bne     LA749
LA764:  clc
        rts

LA766:  sec
        rts

print_found:
        lda     $9D
        bpl     LA7A7
        ldy     #$63 ; "FOUND"
        jsr     print_kernal_string
        ldy     #5
LA773:  lda     ($B2),y
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     #$15
        bne     LA773
        rts

LA77E:  jsr     LA7B1
        bmi     LA796
        rts

print_searching:
        lda     $9D
        bpl     LA7A7
        ldy     #$0C ; "SEARCHING"
        jsr     print_kernal_string
        lda     $B7
        beq     LA7A7
        ldy     #$17 ; "FOR"
        jsr     print_kernal_string
LA796:  ldy     $B7
        beq     LA7A7
        ldy     #0
LA79C:  jsr     _load_FNADR_indy
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     $B7
        bne     LA79C
LA7A7:  rts

print_loading:
        ldy     #$49 ; "LOADING"
        lda     $93
        beq     LA7B3
        ldy     #$59 ; "VERIFYING"
        .byte   $2C
LA7B1:  ldy     #$51 ; "SAVING"
LA7B3:  bit     $9D
        bpl     LA7C4
print_kernal_string:
        lda     $F0BD,y ; KERNAL strings
        php
        and     #$7F
        jsr     $E716 ; KERNAL: output character to screen
        iny
        plp
        bpl     print_kernal_string ; until MSB set
LA7C4:  clc
        rts

; ----------------------------------------------------------------
; tape related

.segment "tape"

; ??? unused?
loc_A7C6:
        ldx     #load_ac_indy_end - load_ac_indy - 1
loc_A7C8:
        lda     load_ac_indy,x
        sta     L0110,x
        dex
        bpl     loc_A7C8
        ldx     #5
        stx     $AB
        jsr     $FB8E ; copy I/O start address to buffer address
        jsr     LA75B
        bcc     loc_A7E2
        lda     #0
        jmp     _disable_rom
loc_A7E2:
        jsr     LA77E
        jsr     turn_screen_off
        jsr     LA999
        lda     SA
        clc
        adc     #1
        dex
        jsr     LA9BB
        ldx     #8
loc_A7F6:
        lda     $AC,y
        jsr     LA9BB
        ldx     #6
        iny
        cpy     #5
        nop
        bne     loc_A7F6
        ldy     #0
        ldx     #2
LA808:  jsr     _load_FNADR_indy
        cpy     $B7
        bcc     loc_A812
        lda     #$20
        dex
loc_A812:
        jsr     LA9BB
        ldx     #3
        iny
        cpy     #$BB
        bne     LA808
        lda     #2
        sta     $AB
        jsr     LA999
        tya
        jsr     LA9BB
        sty     $D7
        ldx     #5
LA82B:  jsr     L0110
        ldx     #3 ; used to be "#2" in 1988-05
        inc     $AC
        bne     loc_A837
        inc     $AD
        dex
loc_A837:
        lda     $AC
        cmp     $AE
        lda     $AD
        sbc     $AF
        bcc     LA82B
LA841:  lda     $D7
        jsr     LA9BB
        ldx     #7
        dey
        bne     LA841
        jsr     LA912
        jmp     _disable_rom

LA851:  jsr     LA8C9
        lda     $AB
        cmp     #2
        beq     LA862
        cmp     #1
        bne     LA851
        lda     SA
        beq     LA86C ; "LOAD"[...]",n,0" -> skip load address
LA862:  lda     $033C
        sta     $C3
        lda     $033D
        sta     $C4
LA86C:  jsr     print_found
        cli
        lda     $A1
        jsr     $E4E0 ; wait for CBM key
        sei
        lda     $01
        and     #$1F
        sta     $01
        ldy     $B7
        beq     LA88C
LA880:  dey
        jsr     _load_FNADR_indy
        cmp     $0341,y
        bne     LA851
        tya
        bne     LA880
LA88C:  sty     ST
        jsr     print_loading
        lda     $C3
        sta     $AC
        lda     $C4
        sta     $AD
        sec
        lda     $033E
        sbc     $033C
        php
        clc
        adc     $C3
        sta     $AE
        lda     $033F
        adc     $C4
        plp
        sbc     $033D
        sta     $AF
        jsr     LA8E5
        lda     $BD
        eor     $D7
        ora     ST
        clc
        beq     LA8C2
        sec
        lda     #$FF
        sta     ST
LA8C2:  ldx     $AE
        ldy     $AF
        jmp     _disable_rom

LA8C9:  jsr     LA92B
        lda     $BD
        cmp     #0 ; XXX not needed
        beq     LA8C9
        sta     $AB
LA8D4:  jsr     LA96E
        lda     $BD
        sta     ($B2),y
        iny
        cpy     #$C0
        bne     LA8D4
        beq     LA913
LA8E2:  jmp     L0110

LA8E5:  jsr     LA92B
LA8E8:  jsr     LA96E
        cpy     $93
        bne     LA8E2
        lda     #$0B
        sta     $01
        lda     $BD
        sta     ($C3),y
        eor     $D7
        sta     $D7
        lda     #$0F
        sta     $01
LA8FF:
        inc     $C3
        bne     LA905
        inc     $C4
LA905:  lda     $C3
        cmp     $AE
        lda     $C4
        sbc     $AF
        bcc     LA8E8
        jsr     LA96E
LA912:  iny
LA913:  sty     $C0
        lda     #0
        sta     $02A0
        lda     $D011
        ora     #$10
        sta     $D011 ; turn screen on
        lda     $01
        ora     #$20
        sta     $01
        cli
        clc
        rts

LA92B:  jsr     LA742
        bcc     LA939
        pla
        pla
        pla
        pla
        lda     #0
        jmp     _disable_rom

LA939:  jsr     turn_screen_off
        sty     $D7
        lda     #$07
        sta     $DD06
        ldx     #1
LA945:  jsr     LA97E
        rol     $BD
        lda     $BD
        cmp     #2
        beq     LA954
        cmp     #$F2
        bne     LA945
LA954:  ldy     #9
LA956:  jsr     LA96E
        lda     $BD
        cmp     #2
        beq     LA956
        cmp     #$F2
        beq     LA956
LA963:  cpy     $BD
        bne     LA945
        jsr     LA96E
        dey
        bne     LA963
        rts

LA96E:  lda     #8
        sta     $A3
LA972:  jsr     LA97E
        rol     $BD
        nop
        nop
        dec     $A3
        bne     LA972
        rts

LA97E:  lda     #$10
LA980:  bit     $DC0D
        beq     LA980
        lda     $DD0D
        stx     $DD07
        pha
        lda     #$19
        sta     $DD0F
        pla
        lsr     a
        lsr     a
        rts

        lda     #4
        sta     $AB
LA999:  ldy     #0
LA99B:  lda     #2
        jsr     LA9BB
        ldx     #7
        dey
        cpy     #9
        bne     LA99B
        ldx     #5
        dec     $AB
        bne     LA99B
LA9AD:  tya
        jsr     LA9BB
        ldx     #7
        dey
        bne     LA9AD
        dex
        dex
        sty     $D7
        rts

LA9BB:  sta     $BD
        eor     $D7
        sta     $D7
        lda     #8
        sta     $A3
LA9C5:  asl     $BD
        lda     $01
        and     #$F7
        jsr     LA9DD
        ldx     #$11
        nop
        ora     #8
        jsr     LA9DD
        ldx     #14
        dec     $A3
        bne     LA9C5
        rts

LA9DD:  dex
        bne     LA9DD
        bcc     LA9E7
        ldx     #11
LA9E4:  dex
        bne     LA9E4
LA9E7:  sta     $01
        rts

turn_screen_off:
        ldy     #0
        sty     $C0
        lda     $D011
        and     #$EF
        sta     $D011 ; turn screen off
LA9F6:  dex
        bne     LA9F6 ; delay (XXX waiting for $D012 == 0 would be cleaner)
        dey
        bne     LA9F6
        sei
        rts

; XXX junk
  .byte $FF,$FF
