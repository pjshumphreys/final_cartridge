.segment "menu_start"

menu:
  ldx #$ff
  CLD             ; clear direction flag
  txs
  STX $D016       ; sets bit 5 (MCM) off, bit 3 (38 cols) off
  lda #$15
  sta $d018
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
  sta $cc     ; Force cursor to flash
  LDA #$80    ; disable shift character set
  STA $0291
  cli         ; Re-enable IRQ interrupts

getChar2:
  jsr $FFE4; GETIN
  cmp #$31
  beq fc3b
  cmp #$32
  beq simons
  cmp #$33
  beq cbmb
  cmp #$34
  beq copySL
  cmp #$35
  beq geos
  jmp getChar2

simons:
  jmp simons2

geos:
  jmp geos2

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
  lda #0
  sta $8004
  lda #$FC
  pha
  lda #$E1
  pha
  lda #$70
  jmp $DF00 ;soft reset the machine

copySL:
  jsr $ffd2      ; print the character we found
  lda #4
  jsr waitForNoKey
  sei
  lda #0
  sta $fb
  sta $fd
  lda #>swiftlink
  sta $fc
  lda #08
  sta $fe
  ldy #$ff

copySL2:
  lda ($fb), y
  sta ($fd), y
  dey
  bne copySL2
  inc $fc
  inc $fe
  lda $fc
  cmp #$12
  beq copySL3
  jmp copySL2

copySL3:
  lda #11
  sta $D020
  lda #15
  sta $0286
  sta $0287
  lda #$08
  sta $49
  sta $ba
  lda #0
  sta $D021
  ldx #0
startup3:
  lda startup, x
  sta $8000, x
  inx
  cpx #$fe
  bcc startup3
  lda #<$EA31
  sta $0314
  lda #>$EA31
  sta $0315
  lda #$FC
  pha
  lda #$E1
  pha
  lda #$70
  jmp $DF00 ;soft reset the machine

geos2:
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

simons2:
  jsr $ffd2      ; print the character we found
  lda #2
  jsr waitForNoKey
  sei
  lda #0
  ldy #15

copyTrampoline:
  lda copyRomFoo-1, y
  sta $02a6, y
  dey
  bne copyTrampoline

  ;copy the 16k
  lda #0
  sta $fb
  sta $fd
  lda #$80
  sta $fc
  sta $fe

copyRom:
  jsr $2a7  ;lda ($fb), y; sta($fd), y
  dey
  bne copyRom
  inc $fc
  inc $fe
  lda $fc
  cmp #$c0
  beq copyHiRom
  jmp copyRom

copyHiRom:
  lda #$a0
  sta $fc

  ;copy the 4k to $c000
copyHiRom2:
  lda ($fb), y
  sta ($fd), y
  dey
  bne copyHiRom2
  ldy #0
  inc $fc
  inc $fe
  lda $fe
  cmp #$d0
  beq startRom
  jmp copyHiRom2

startRom:
  lda #$4c ; jmp
  sta $02AC
  lda #$71
  sta $02AD
  lda #$81
  sta $02AE
  lda #6
  sta $D020
  lda #15
  sta $D021
  lda #0
  sta $0286
  sta $0287
  sta $0291 ; enable shift character set
  lda $BA
  sta $be
  lda #$80
  sta $38

  lda #$70 ; no nmi
  jmp $02A9 ;soft reset the machine

  ;this is copied to $2a7 and run from there
copyRomFoo:
  lda #3
  sta $dfff
  lda ($fb), y
  sta ($fd), y
  lda #1
  sta $dfff
  rts

filterTable:
  .byte %01111111
  .byte %01111111
  .byte %11111101
  .byte %11111101
  .byte %11111011

filterTableA:
  .byte %00000001
  .byte %00001000
  .byte %00000001
  .byte %00001000
  .byte %00000001

menuText:
  .byte $93, $0D, $0D
  .byte "    cHOOSE AN OPTION:-", $0D, $0D, $0D
  .byte "    1 - fINAL cARTRIDGE iii bASIC", $0D, $0D
  .byte "    2 - tUNED sIMONS' bASIC", $0D, $0D
  .byte "    3 - cOMMODORE bASIC", $0D, $0D
  .byte "    4 - sWIFTlINK-232 fILE tRANSFER", $0D, $0D
  .byte "    5 - bOOT geos", $0D, $0D, $0D
  .byte "    eNTER OPTION NUMBER (1 - 5): ", $00

.align 256
startup:
  .byte $09, $80, $bc, $82, $c3, $c2, $cd, $38, $30
  stx $d016
  jsr $fda3
  ;jsr $fd50
  ldx #0
startup2:
  lda basicSize-$300, x
  sta $2b, x
  inx
  cpx #$08
  bne startup2
  lda #$52
  sta $277
  lda #$55
  sta $278
  lda #$4e
  sta $279
  lda #$0d
  sta $27a
  lda #$41
  sta $030c
  lda #0
  sta $030d
  sta $030e
  sta $030f
  ldx #$04
  stx $c6
  jmp $E3bf

basicSize:
  .byte $01, $08, $4e, $1b, $4e, $1b, $4e, $1b

.align 256

swiftlink:
.byte $ff
.incbin "swiftlink.bin"

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
.incbin "page3a.bin"

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

