!cpu 6502

*= $0400
!src "fire.asm"

*= $0800
!byte $00, $0b, $08, $01, $00, $9e, $34, $30, $39, $36

*= $1000

scrbuf1 = $0400
scrbuf2 = $0800

main:
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

initScreen:
    lda #$00
    sta $d020
    sta $d021
    rts

;============================================================
;    custom interrupt routine
;============================================================

.xscroll !fill 1, $08
irq:
    dec $d019        ; acknowledge IRQ

    ;lda #$0a
    ;sta $d020   ; change frame color for assesing how much time interrupt took

    jsr decxscroll
    lda .xscroll
    beq .shiftOneChar ; we cycled through values 7 to 0 so we need to shift the whole screen one char position
    cmp #$07
    beq .swapBuf    ; if xscroll is 7 we shifted all chars by one position one irq before
                    ; so we need to change the displayed buffer
    jmp .exit

.swapBuf
    jsr swapBuffers
    jmp .exit

.shiftOneChar
    lda #$08
    sta .xscroll
    jsr shiftScreen ; and shift entire screen by one character

.exit
    ;lda #$05
    ;sta $d020   ; reset frame color for assesing how much time irq took
    jmp $ea81   ; return to kernel interrupt routine

decxscroll:
    dec .xscroll
    lda $d016   ; if not, we load a register that holds x scroll offset
    and #$f8    ; mask off appropriate values (4 most significant bits)
    clc
    adc .xscroll    ; add our scroll value
    sta $d016   ; and store it back
    rts

!zone swapBuffers
swapBuffers:
    lda currentCycle
    bne .secondCycle
    lda $d018
    and #!$20   ; turn off bit for buffer at $0400
    ora #$10    ; turn on bit for buffer at $0800
    sta $d018
    jmp .exit
.secondCycle
    lda $d018
    and #!$10   ; turn off bit for buffer at $0800
    ora #$20    ; turn on bit for buffer at $0400
    sta $d018
.exit:
    rts

!zone shiftScreen
currentCycle !fill 1, $00   ; 0 or 1, depends on current front and backbuffer
shiftScreen:
    lda currentCycle
    bne .secondCycle

    ; call appropriate subroutine for shifting screen to an appropriate framebuffer
    jsr shiftToBuf2
    lda #$01
    sta currentCycle    ; change current cycle to opposite
    jmp .exit

.secondCycle
    lda #$00
    sta currentCycle    ; change current cycle to opposite
    jsr shiftToBuf1

.exit
    rts

shiftToBuf2:
    ldx #$00
.loop
    ; copy characters from scrbuf1 to scrbuf2 offset one character to the left
    ; disregard characters that would wrap around from the right for now
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

    ; correctly copy characters that should wrap around, from the left edge to the the right edge
    !for i, 0, 24 {
        lda scrbuf1 + i * 40
        sta scrbuf2 - 1 + (i + 1) * 40
    }
    rts

!zone shiftToBuf1
shiftToBuf1:
    ldx #$00
.loop
    ; same as above but copies from scrbuf2 to scrbuf1
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