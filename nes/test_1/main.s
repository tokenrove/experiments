;
; quick test NES demo for homebrew
;
; tokenrove / 2003
;

        .segment "CODE"

        .import _write_foo

.proc reset
        cld
        sei
        ldx #$ff
        txs

        jsr init_ppu
        cli
:       jmp :-
.endproc ; reset


.proc nmi
nmi:    sei
        pha

        lda $02
        sta $2005
        lda #$00
        sta $2005
        inc $02

        pla
        cli
        rti
.endproc ; nmi

irq:    rti

init_ppu:
        ; disable display
	lda #$00
	sta $2000
	sta $2001

        ; copy font to pattern table
        ldx #$10
        tay
        sty $2006
        sty $2006
        lda #<font
        sta $00
        lda #>font
        sta $01

:       lda ($00),Y
        sta $2007
        iny
        bne :-

        inc $01
        dex
        bne :-

        ; write some stuff onto the screen
        jsr _write_foo

        ; load palette
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	tax

: 	lda bgpalette, x
	sta $2007
	inx
	cpx #$10
	bne :-

        ; init display
	lda #%10001000
	sta $2000
	lda #%00001010
	sta $2001
        rts

        .segment "RODATA"

font:   .incbin "font.dat"

bgpalette:
	.byte $0d, $00, $00, $12, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00


        .segment "VECTORS"

        .word nmi, reset, irq

; EOF
