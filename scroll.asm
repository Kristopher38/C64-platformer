!cpu 6502

*= $0400
!src "fire.asm"

*= $0800
!byte $00, $0b, $08, $01, $00, $9e, $34, $30, $39, $36

*= $1000

scrbuf1 = $0400
scrbuf2 = $0800

jsr initScreen

sei         ; set interrupt disable flag

ldy #$7f    ; $7f = %01111111
sty $dc0d   ; Turn off CIAs Timer interrupts
sty $dd0d   ; Turn off CIAs Timer interrupts
lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed

lda #$01    ; Set Interrupt Request Mask...
sta $d01a   ; ...we want IRQ by Rasterbeam

lda #<irq   ; point IRQ Vector to our custom irq routine
ldx #>irq 
sta $314    ; store in $314/$315
stx $315   

lda #$00    ; trigger first interrupt at row zero
sta $d012

lda $d011   ; Bit#0 of $d011 is basically...
and #$7f    ; ...the 9th Bit for $d012
sta $d011   ; we need to make sure it is set to zero 

cli         ; clear interrupt disable flag
jmp *       ; infinite loop

;============================================================
;    custom interrupt routine
;============================================================

.xscroll !fill 1, $07
irq:
dec $d019        ; acknowledge IRQ

lda #$0a
sta $d020

lda .xscroll
cmp #$07
beq .blit
jmp .decscroll

.blit
lda currentCycle
bne .secondCycle
lda $d018
and #!$20
ora #$10
sta $d018
jmp .decscroll

.secondCycle
lda $d018
and #!$10
ora #$20
sta $d018

.decscroll
dec .xscroll
beq .shift
lda $d016
and #$f8
clc
adc .xscroll
sta $d016
jmp .exit

.shift
lda #$07
sta .xscroll
jsr shiftScreen

.exit
lda #$05
sta $d020

jmp $ea81        ; return to kernel interrupt routine


initScreen:
;lda $d016
;ora #$07
rts

!zone shiftScreen
currentCycle !fill 1, $00
shiftScreen:
lda currentCycle
bne .secondCycle

jsr shiftToBuf2
lda #$01
sta currentCycle
jmp .exit

.secondCycle
lda #$00
sta currentCycle
jsr shiftToBuf1

.exit
rts

shiftToBuf2:
ldx #$00
.loop
    lda scrbuf1 + $001, x
    sta scrbuf2 + $000, x
    lda scrbuf1 + $101, x
    sta scrbuf2 + $100, x
    lda scrbuf1 + $201, x
    sta scrbuf2 + $200, x
    lda scrbuf1 + $2e9, x
    sta scrbuf2 + $2e8, x
    inx
    bne .loop

!for i, 0, 24 {
    lda scrbuf1 + i * 40
    sta scrbuf2 - 1 + (i + 1) * 40
}
rts

!zone shiftToBuf1
shiftToBuf1:
ldx #$00
.loop
    lda scrbuf2 + $001, x
    sta scrbuf1 + $000, x
    lda scrbuf2 + $101, x
    sta scrbuf1 + $100, x
    lda scrbuf2 + $201, x
    sta scrbuf1 + $200, x
    lda scrbuf2 + $2e9, x
    sta scrbuf1 + $2e8, x
    inx
    bne .loop

!for i, 0, 24 {
    lda scrbuf2 + i * 40
    sta scrbuf1 - 1 + (i + 1) * 40
}
rts

!zone delay
delay:
ldx $00
.loop
    !for i, 0, 5 {
        nop
    }
    inx
    bne .loop
rts