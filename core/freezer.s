; ----------------------------------------------------------------
; Freezer Entry
; ----------------------------------------------------------------
; In Ultimax mode, we have the following memory layout:
; $8000-$9FFF: bank 0 lo
; $E000-$FFFF: bank 0 hi
; This code is mapped into bank 0 hi, and the vectors appear
; at the very end of this bank.
; The code here only does some minimal saving of state, then
; jumps to a different bank.

.include "persistent.i"

.segment "freezer"

.segment "freezer_vectors"
.import nmiVec
.import resetVec
.import irqVec

; catch IRQ, NMI, RESET
  .word nmiVec   ; NMI vector
  .word resetVec   ; RESET vector
  .word irqVec   ; IRQ/BRK vector

