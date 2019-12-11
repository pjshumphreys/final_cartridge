.segment "menu_start"

menu:
  ldx #$ff
  CLD             ; clear direction flag
  STX $D016       ; sets bit 5 (MCM) off, bit 3 (38 cols) off
  JSR $FDA3       ; initialise I/O
  JSR $FD15       ; set I/O vectors ($0314..$0333) to kernal defaults
  jsr $FF81       ; CINT   - Init VIC and screen editor
  JSR $FF5B       ; more initialising... mostly set system IRQ to correct value and start
  CLI             ; clear interrupt flag
  ldy #$00
  lda #<menuText
  sta $FB
  lda #>menuText
  sta $FC
  lda $D018
  and #$02
  cmp #0
  bne printChar
  lda #14
  jsr $ffd2

printChar:
  lda ($00fb), y
  cmp #0
  beq getChar
  jsr $ffd2 ; CHROUT
  iny
  bne printChar
  inc $fb
  jmp printChar

getChar:
  lda #$00
  sta $cc        ; Force cursor to flash
  LDA #$80  ;disable shift character set
  STA $0291
  cli         ; Re-enable IRQ interrupts

getChar2:
  jsr $FFE4; GETIN
  cmp #$31
  beq fc3b
  cmp #$32
  beq quit
  cmp #$33
  beq cbmb
  cmp #$34
  beq quit
  cmp #$35
  beq geos
  jmp getChar2

fc3b:
  jsr $ffd2      ; print the character we found
  lda #1
  jsr waitForNoKey
  sei
  lda #$FC
  pha
  lda #$E1
  pha
  lda #0
  jmp $DF00 ;soft reset the machine

cbmb:
  jsr $ffd2      ; print the character we found
  lda #3
  jsr waitForNoKey
  sei
  lda #$FC
  pha
  lda #$E1
  pha
  lda #$70
  jmp $DF00 ;soft reset the machine

geos:
  jsr $ffd2      ; print the character we found
  lda #5
  jsr waitForNoKey
  sei

  lda #$8d ; sta
  sta $02
  lda #$FF
  sta $03
  lda #$DF
  sta $04
  lda #$4c ; jmp
  sta $05
  lda #$00
  sta $06
  lda #$80
  sta $07
  lda #$42
  jmp $02 ;soft reset the machine

quit:
  jsr $ffd2
quit2:
  sei
  jmp quit2

IRQ:
  JSR $FFEA   ; do clock
  jmp $ea38


waitForNoKey:
  sei
  tay
  lda #$0c
  sta $cc   ; disable cursor
  lda #$0
  sta $dc03 ; port b ddr (input)
  lda #$ff
  sta $dc02 ; port a ddr (output)

waitForNoKey2:
  lda filterTable-1, y
  sta $dc00 ; port a
  lda $dc01       ; port b
  and filterTableA-1, y
  cmp #0
  beq waitForNoKey2
  rts

filterTable:
  .byte %01111111
  .byte $ff
  .byte %11111101
  .byte $ff
  .byte %11111011

filterTableA:
  .byte %00000001
  .byte $ff
  .byte %00000001
  .byte $ff
  .byte %00000001

menuText:
  .byte $0D, $0D
  .byte "    cHOOSE AN OPTION:-", $0D, $0D, $0D
  .byte "    1 - fINAL cARTRIDGE iii bASIC", $0D, $0D
  .byte "    2 - tUNED sIMONS' bASIC", $0D, $0D
  .byte "    3 - cOMMODORE bASIC", $0D, $0D
  .byte "    4 - sWIFTlINK-232 fILE tRANSFER", $0D, $0D
  .byte "    5 - bOOT geos", $0D, $0D, $0D
  .byte "    eNTER OPTION NUMBER (1 - 5): ", $00

.segment "fc_vectors"
_jmp_bank:
  sta     $DFFF
  rts

.global _disable_rom_set_01
_disable_rom_set_01:; $DE0D
        sty     $01

.global _disable_rom
_disable_rom: ; $DE0F
        pha
        lda     #$70 ; no ROM at $8000; BASIC at $A000
LDE08:  sta     $DFFF
        pla
        rts

.segment "himem"
.byte $ff

.segment "ultimax"
nmiVec:
  sei
  lda #$7F
  pha
  lda #$FF
  pha
  lda #1
  jmp $DF00

.segment "untimax_vectors"
; catch IRQ, NMI, RESET
  .word nmiVec   ; NMI vector
  .word nmiVec   ; RESET vector
  .word nmiVec   ; IRQ/BRK vector

