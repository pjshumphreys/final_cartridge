.include "kernal.i"
.include "persistent.i"

L0100           := $0100
L0110           := $0110
L04F6           := $04F6
L0582           := $0582

.segment "part2b"

.global new_load
new_load: ; $9900
	jmp new_load2
.global new_save
new_save: ; $9903
	jmp new_save2

L9906:  pha
L9907:  bit     $DD00
        bpl     L9907
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        tax
L9911:  lda     $D012
        cmp     #$31
        bcc     L991E
        and     #$06
        cmp     #$02
        beq     L9911
L991E:  lda     #$07
        sta     $DD00
        lda     L994B,x
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
        lda     L994B,x
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

L994B:  .byte   $07,$87,$27,$A7,$47,$C7,$67,$E7
        .byte   $17,$97,$37,$B7,$57,$D7,$77,$F7

L995B:  lda     $0330
        cmp     #<_new_load
        beq     L998B
L9962:  bit     $DD00
        bvs     L9962
        ldy     #$03
        nop
        ldx     $01
L996C:  lda     $DD00
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
        bpl     L996C
        rts

L998B:  bit     $DD00
        bvs     L998B
        ldy     #$03
        nop
        ldx     $01
L9995:  lda     $DD00
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

L99B5:  tax
        beq     L99C3
        ldx     #$16
L99BA:  lda     L9A50,x
        sta     L0110,x
        dex
        bpl     L99BA
L99C3:  jmp     LA851

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
        ldy     $BA
        cpy     #$07
        beq     L99B5
        cpy     #$08
        bcc     L99C9
        cpy     #$0A
        bcs     L99C9
        tay
        lda     $B7
        beq     L99C9
        jsr     _load_bb_indy
        cmp     #$24
        beq     L99C9
        ldx     $B9
        cpx     #$02
        beq     L99C9
        jsr     LA784
        lda     #$60
        sta     $B9
        jsr     LA71B
        lda     $BA
        jsr     $ED09 ; TALK
        lda     $B9
        jsr     $EDC7 ; SECTLK
        jsr     $EE13 ; IECIN
        sta     $AE
        lda     $90
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
        cmp     #$04
        bcc     L99D6
        jmp     L9AF0

; ----------------------------------------------------------------

.segment "code_at_0100"

; will be placed at $0100
L9A41:
        lda     #$0C
        sta     $01
        lda     ($AC),y
        ldy     #$0F
        sty     $01
        ldy     #$00
        jmp     LA9BB

.segment "code_at_0110"

L9A50:  lda     #$0C
        sta     $01
        lda     ($C3),y
        cmp     $BD
        beq     L9A5C
        stx     $90
L9A5C:  eor     $D7
        sta     $D7
        lda     #$0F
        sta     $01
        jmp     LA8FF

.segment "part3"

L9A67:  jmp     $F636 ; LDA #0 : SEC : RTS

L9A6A:  jmp     $F5ED ; default SAVE vector

L9A6D:  jmp     $A7C6 ; interpreter loop

new_save2:
        lda     $BA
        cmp     #$07
        beq     L9A6D ; tape turbo
        cmp     #$08
        bcc     L9A6A ; not a drive
        cmp     #$0A
        bcs     L9A6A ; not a drive (XXX why only support drives 8 and 9?)
        ldy     $B7
        beq     L9A6A
        lda     #$61
        sta     $B9
        jsr     LA71B
        jsr     LA77E
        jsr     LA648
        bne     L9A67
        stx     $90
        stx     $A4
        jsr     $FB8E ; copy I/O start address to buffer address
        sec
        lda     $AC
        sbc     #$02
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

L9AC7:  jsr     L9906
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
L9AED:  jmp     L9906

L9AF0:  jsr     UNTALK
        jsr     LA691
        lda     #$06
        sta     $93
.import __drive_code_LOAD__
.import __drive_code_RUN__
        lda     #<__drive_code_LOAD__
        ldy     #>__drive_code_LOAD__
        ldx     #$04 ; $0400
        jsr     transfer_code_to_drive
        lda     #$9A
        jsr     IECOUT
        lda     #$05
        jsr     IECOUT
        jsr     UNLSTN
        sei
        lda     $D011
        tax
        and     #$10
        sta     $95
        txa
        and     #$EF
        sta     $D011
        lda     $DD00
        and     #$07
        ora     $95
        sta     $95
        lda     $C1
        sta     $A4
        lda     $C2
        sta     $B9
        sec
        lda     $AE
        sbc     #$02
        sta     $90
        lda     $AF
        sbc     #$00
        sta     $A3
L9B3D:  bit     $DD00
        bmi     L9B82
        cli
        php
        lda     $95
        and     #$07
        sta     $DD00
        lda     $95
        and     #$10
        ora     $D011
        sta     $D011
        lda     $A4
        sta     $C1
        lda     $B9
        sta     $C2
        lda     #$00
        sta     $A3
        sta     $94
        sta     $90
        lda     #$60
        sta     $B9
        lda     #$E0
        jsr     LA612
        jsr     UNLSTN
        plp
        bvs     L9B78 ; used to be "bcs" in 1988-05
        lda     #$1D
        sec
        rts

L9B78:  lda     #$40
        sta     $90
        jsr     LA694
        jmp     $F5A9 ; LOAD done

L9B82:  bvs     L9B3D
        lda     #$20
        sta     $DD00
L9B89:  bit     $DD00
        bvc     L9B89
        lda     #$00
        sta     $DD00
        jsr     L995B
        lda     #$FE
        sta     $A5
        lda     $C3
        clc
        adc     $A3
        tax
        asl     $C3
        php
        sec
        lda     $90
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
        adc     #$00
        sta     $AF
L9BC8:  ldy     #$00
        lda     $C3
        bne     L9BD7
        jsr     L995B
        ldy     #$02
        ldx     #$02
        bne     L9BE5
L9BD7:  lda     $C1
        sta     ($93),y
        iny
L9BDC:  tya
        pha
        jsr     L995B
        pla
        tay
        ldx     #$03
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

.segment "drive_code"
; drive code
drive_code:
        lda     $43
        sta     $C1
L9BFE:  jsr     L0582
L9C01:  bvc     L9C01
        clv
        lda     $1C01
        sta     $25,y
        iny
        cpy     #$07
        bne     L9C01
        jsr     $F556 ; drive ROM
L9C12:  bvc     L9C12
        clv
        lda     $1C01
        sta     ($30),y
        iny
        cpy     #$05
        bne     L9C12
        jsr     $F497 ; drive ROM
        ldx     #$05
        lda     #$00
L9C26:  eor     $15,x
        dex
        bne     L9C26
        tay
        beq     L9C31
L9C2E:  jmp     $F40B ; drive ROM

L9C31:  inx
L9C32:  lda     $12,x
        cmp     $16,x
        bne     L9C2E
        dex
        bpl     L9C32
        jsr     $F7E8 ; drive ROM
        ldx     $19
        cpx     $43
        bcs     L9C2E
        lda     $53
        sta     $060F,x
        lda     $54
        sta     $05FA,x
        lda     #$FF
        sta     $0624,x
        dec     $C1
        bne     L9BFE
        lda     #$01
        sta     $C3
        ldx     $09
L9C5D:  lda     $C2
        sta     $0624,x
        inc     $C2
        lda     $060F,x
        cmp     $08
        bne     L9C75
        lda     $05FA,x
        tax
        inc     $C3
        bne     L9C5D
        beq     L9C2E
L9C75:  cmp     #$24
        bcs     L9C2E
        sta     $08
        lda     $05FA,x
        sta     $09
L9C80:  jsr     L0582
        iny
L9C84:  bvc     L9C84
        clv
        lda     $1C01
        sta     ($30),y
        iny
        cpy     #$04
        bne     L9C84
        ldy     #$00
        jsr     $F7E8 ; drive ROM
        ldx     $54
        cpx     $43
        bcs     L9C2E
        lda     $0624,x
        cmp     #$FF
        beq     L9C80
        stx     $C0
        jsr     $F556 ; drive ROM
L9CA8:  bvc     L9CA8
        clv
        lda     $1C01
        sta     ($30),y
        iny
        bne     L9CA8
        ldy     #$BA
L9CB5:  bvc     L9CB5
        clv
        lda     $1C01
        sta     L0100,y
        iny
        bne     L9CB5
        jsr     $F7E8 ; drive ROM
        lda     $53
        beq     L9CCC
        lda     #$00
        sta     $54
L9CCC:  sta     $34
        sta     $C1
        ldx     $C0
        lda     $0624,x
        sta     $53
        lda     #$FF
        sta     $0624,x
        jsr     $F6D0 ; drive ROM
        lda     #$42
        sta     $36
        ldy     #$08
        sty     $1800
L9CE8:  lda     $1800
        lsr     a
        bcc     L9CE8
        ldy     #$00
        dec     $36
        sty     $1800
        bne     L9CFE
        dec     $C3
        bne     L9C80
        jmp     $F418 ; drive ROM

L9CFE:  ldy     $C1
        lda     ($30),y
        lsr     a
        lsr     a
        lsr     a
        sta     $5C
        lda     ($30),y
        and     #$07
        sta     $5D
        iny
        bne     L9D15
        iny
        sty     $31
        ldy     #$BA
L9D15:  lda     ($30),y
        asl     a
        rol     $5D
        asl     a
        rol     $5D
        lsr     a
        lsr     a
        lsr     a
        sta     $5A
        lda     ($30),y
        lsr     a
        iny
        lda     ($30),y
        rol     a
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$1F
        sta     $5B
        lda     ($30),y
        and     #$0F
        sta     $58
        iny
        lda     ($30),y
        asl     a
        rol     $58
        lsr     a
        lsr     a
        lsr     a
        sta     $59
        lda     ($30),y
        asl     a
        asl     a
        asl     a
        and     #$18
        sta     $56
        iny
        lda     ($30),y
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$07
        ora     $56
        sta     $56
        lda     ($30),y
        and     #$1F
        sta     $57
        iny
        sty     $C1
        ldy     #$08
        sty     $1800
        ldx     $55,y
L9D68:  lda     $05C2,x
        sta     $1800
        lda     $05DA,x
        ldx     $54,y
        sta     $1800
        dey
        bne     L9D68
        jmp     L04F6

        ldx     #$03
        stx     $31
L9D80:  inx
        bne     L9D86
        jmp     $F40B ; drive ROM

L9D86:  jsr     $F556 ; drive ROM
L9D89:  bvc     L9D89
        clv
        lda     $1C01
        cmp     $24
        bne     L9D80
        rts

        ldx     #$00
        stx     $1800
        stx     $C2
        lda     $19
        sta     $09
        lda     $18
        sta     $08
L9DA3:  lda     #$E0
        sta     $01
L9DA7:  lda     $01
        bmi     L9DA7
        cmp     #$02
        bcs     L9DBB
        lda     $08
        bne     L9DA3
        lda     #$02
        sta     $1800
        jmp     $C194 ; drive ROM

L9DBB:  inx
        ldy     #$0A
        sty     $1800
        jmp     $E60A ; drive ROM

; ??? unreferenced?
        .byte   0, 10, 10, 2
        .byte   0, 10, 10, 2
        .byte   0, 0, 8, 0
        .byte   0, 0, 8, 0
        .byte   0, 2, 8, 0
        .byte   0, 2, 8, 0
        .byte   0, 8, 10, 10, 0, 0, 2, 2
        .byte   0, 0, 10, 10, 0, 0, 2, 2
        .byte   0, 8, 8, 8
        .byte   0, 0, 0, 0

; ----------------------------------------------------------------
; drive code $0500
; ----------------------------------------------------------------
.segment "drive_code2"

drive_code2:
        lda     L0612
        tax
        lsr     a
        adc     #$03
        sta     $95
        sta     $31
        txa
        adc     #$06
        sta     $32
LA510:  jsr     receive_byte
        beq     :+
        sta     $81
        tax
        inx
        stx     L0611
        lda     #$00
        sta     $80
        beq     LA534

:       lda     $02FC
        bne     :+
        lda     $02FA ; XXX ORing the values together is shorter
        bne     :+
        lda     #$72
        jmp     $F969 ; DISK FULL

:       jsr     $F11E ; find and allocate free block
LA534:  ldy     #$00
        sty     $94
        lda     $80
        sta     ($94),y
        iny
        lda     $81
        sta     ($94),y
        iny
LA542:  jsr     $0564
        sta     ($30),y
        iny
        cpy     L0611
        bne     LA542
        jsr     $0150
        inc     $B6
        ldx     L0612
        lda     $81
        sta     $07,x
        lda     $80
        cmp     $06,x
        beq     LA510
        sta     $06,x
        jmp     $F418 ; set OK code

receive_byte:
        lda     #$00
        sta     $1800
        lda     #$04
:       bit     $1800
        bne     :-
        sta     $C0
drive_code2_timing_selfmod1:
        sta     $C0
        lda     $1800
        asl     a
        nop
        nop
        ora     $1800
        asl     a
        asl     a
        asl     a
        asl     a
        sta     a:$C0 ; 16 bit address for timing!
        lda     $1800
        asl     a
        nop
L0589:
        nop
L058A:
        ora     $1800
        and     #$0F
        ora     $C0
        sta     $C0
        lda     #$02
        sta     $1800
        lda     $C0
        rts
L0589_end:
        nop ; filler, gets overwritten when L0589 gets copied down by 1 byte

L059C:
        lda     #$EA
        sta     drive_code2_timing_selfmod1
        sta     drive_code2_timing_selfmod1 + 1 ; insert 1 cycle into code
        ldx     #L0589_end - L0589 - 1
LA5A6:  lda     L0589,x
        sta     L058A,x ; insert 3 cycles into code
        dex
        bpl     LA5A6
L05AF:
        ldx     #$64
LA5B1:  lda     $F575 - 1,x; copy "write data block to disk" to RAM
        sta     $0150 - 1,x
        dex
        bne     LA5B1
        lda     #$60
        sta     $01B4 ; add RTS at the end, just after GCR decoding
        inx
        stx     $82
        stx     $83
        jsr     $DF95 ; drive ROM
        inx
        stx     $1800
LA5CB:  inx
        bne     LA5CB
        sta     L0612 + 1
        asl     a
        sta     L0612
        tax
        lda     #$40
        sta     $02F9
LA5DB:  lda     $06,x
        beq     LA5FA
        sta     $0A
        lda     #$E0
        sta     $02
LA5E5:  lda     $02
        bmi     LA5E5
        cmp     #$02
        bcc     LA5DB
        cmp     #$72
        bne     LA5F4
        jmp     $C1C8 ; set error message

LA5F4:  ldx     L0612 + 1
        jmp     $E60A

LA5FA:  ldx     #L0608_end - L0608
LA5FC:  lda     L0608 - 1,x
        sta     $0150 - 1,x
        dex
        bne     LA5FC
        jmp     $0150

L0608:
        jsr     $DBA5 ; write directory entry
        jsr     $EEF4 ; write BAM
        jmp     $D227 ; close channel
L0608_end:

L0611:
        .byte   0
L0612:


; ----------------------------------------------------------------
; C64 IEC code
; ----------------------------------------------------------------
.segment "part5"

LA612:  pha
        lda     $BA
        jsr     LISTEN
        pla
        jmp     SECOND

LA61C:  lda     #$6F
        pha
        lda     $BA
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
        lda     #$07
        sta     $93
.import __drive_code2_LOAD__
.import __drive_code2_RUN__
        lda     #<__drive_code2_LOAD__
        ldy     #>__drive_code2_LOAD__
        ldx     #$05
        jsr     transfer_code_to_drive
        lda     $0330
        cmp     #<_new_load
        beq     LA66A ; speeder enabled
        lda     #<L059C
        jsr     IECOUT
        lda     #>L059C
        bne     LA671

LA66A:  lda     #<L05AF
        jsr     IECOUT
        lda     #>L05AF
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
        ldy     #$00
        .byte   $2C
LA694:
        ldy     #$08
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
        cmp     #$0D
        bne     LA6C8 ; read until CR
        jsr     UNTALK
        cpy     #'0' ; = no error
        rts

transfer_code_to_drive:
        sta     $C3
        sty     $C4
        ldy     #$00
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
        ldy     #$00
        sty     $90
        lda     $BA
        jsr     $ED0C ; LISTEN
        lda     $B9
        ora     #$F0
        jsr     $EDB9 ; SECLST
        lda     $90
        bpl     LA734
        pla
        pla
        jmp     $F707 ; DEVICE NOT PRESENT ERROR

LA734:  jsr     _load_bb_indy
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

LA768:  lda     $9D
        bpl     LA7A7
        ldy     #$63
        jsr     LA7B7
        ldy     #$05
LA773:  lda     ($B2),y
        jsr     $E716 ; KERNAL: output character to screen
        iny
        cpy     #$15
        bne     LA773
        rts

LA77E:  jsr     LA7B1
        bmi     LA796
        rts

LA784:
        lda     $9D
        bpl     LA7A7
        ldy     #$0C
        jsr     LA7B7
        lda     $B7
        beq     LA7A7
        ldy     #$17
        jsr     LA7B7
LA796:  ldy     $B7
        beq     LA7A7
        ldy     #$00
LA79C:  jsr     _load_bb_indy
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
LA7B7:  lda     $F0BD,y ; KERNAL strings
        php
        and     #$7F
        jsr     $E716 ; KERNAL: output character to screen
        iny
        plp
        bpl     LA7B7 ; until MSB set
LA7C4:  clc
        rts

        ldx     #$0E
LA7C8:  lda     L9A41,x
        sta     L0110,x
        dex
        bpl     LA7C8
        ldx     #$05
        stx     $AB
        jsr     $FB8E ; copy I/O start address to buffer address
        jsr     LA75B
        bcc     LA7E2
        lda     #$00
        jmp     _disable_rom

LA7E2:  jsr     LA77E
        jsr     LA9EA
        jsr     LA999
        lda     $B9
        clc
        adc     #$01
        dex
        jsr     LA9BB
        ldx     #$08
LA7F6:  lda     $AC,y
        jsr     LA9BB
        ldx     #$06
        iny
        cpy     #$05
        nop
        bne     LA7F6
        ldy     #$00
        ldx     #$02
LA808:  jsr     _load_bb_indy
        cpy     $B7
        bcc     LA812
        lda     #$20
        dex
LA812:  jsr     LA9BB
        ldx     #$03
        iny
        cpy     #$BB
        bne     LA808
        lda     #$02
        sta     $AB
        jsr     LA999
        tya
        jsr     LA9BB
        sty     $D7
        ldx     #$05
LA82B:  jsr     L0110
        ldx     #$03 ; used to be "#$02" in 1988-05
        inc     $AC
        bne     LA837
        inc     $AD
        dex
LA837:  lda     $AC
        cmp     $AE
        lda     $AD
        sbc     $AF
        bcc     LA82B
LA841:  lda     $D7
        jsr     LA9BB
        ldx     #$07
        dey
        bne     LA841
        jsr     LA912
        jmp     _disable_rom

LA851:  jsr     LA8C9
        lda     $AB
        cmp     #$02
        beq     LA862
        cmp     #$01
        bne     LA851
        lda     $B9
        beq     LA86C
LA862:  lda     $033C
        sta     $C3
        lda     $033D
        sta     $C4
LA86C:  jsr     LA768
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
        jsr     _load_bb_indy
        cmp     $0341,y
        bne     LA851
        tya
        bne     LA880
LA88C:  sty     $90
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
        ora     $90
        clc
        beq     LA8C2
        sec
        lda     #$FF
        sta     $90
LA8C2:  ldx     $AE
        ldy     $AF
        jmp     _disable_rom

LA8C9:  jsr     LA92B
        lda     $BD
        cmp     #$00
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
        lda     #$00
        sta     $02A0
        lda     $D011
        ora     #$10
        sta     $D011
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
        lda     #$00
        jmp     _disable_rom

LA939:  jsr     LA9EA
        sty     $D7
        lda     #$07
        sta     $DD06
        ldx     #$01
LA945:  jsr     LA97E
        rol     $BD
        lda     $BD
        cmp     #$02
        beq     LA954
        cmp     #$F2
        bne     LA945
LA954:  ldy     #$09
LA956:  jsr     LA96E
        lda     $BD
        cmp     #$02
        beq     LA956
        cmp     #$F2
        beq     LA956
LA963:  cpy     $BD
        bne     LA945
        jsr     LA96E
        dey
        bne     LA963
        rts

LA96E:  lda     #$08
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

        lda     #$04
        sta     $AB
LA999:  ldy     #$00
LA99B:  lda     #$02
        jsr     LA9BB
        ldx     #$07
        dey
        cpy     #$09
        bne     LA99B
        ldx     #$05
        dec     $AB
        bne     LA99B
LA9AD:  tya
        jsr     LA9BB
        ldx     #$07
        dey
        bne     LA9AD
        dex
        dex
        sty     $D7
        rts

LA9BB:  sta     $BD
        eor     $D7
        sta     $D7
        lda     #$08
        sta     $A3
LA9C5:  asl     $BD
        lda     $01
        and     #$F7
        jsr     LA9DD
        ldx     #$11
        nop
        ora     #$08
        jsr     LA9DD
        ldx     #$0E
        dec     $A3
        bne     LA9C5
        rts

LA9DD:  dex
        bne     LA9DD
        bcc     LA9E7
        ldx     #$0B
LA9E4:  dex
        bne     LA9E4
LA9E7:  sta     $01
        rts

LA9EA:  ldy     #$00
        sty     $C0
        lda     $D011
        and     #$EF
        sta     $D011
LA9F6:  dex
        bne     LA9F6
        dey
        bne     LA9F6
        sei
        rts

; ??? unreferenced?
        sei
        rts