
        .export _write_foo
        .segment "CODE"

.proc _write_foo

	lda #$20
	sta $2006
	lda #$00
	sta $2006
        sta $2007
        ldx #$FF
:       adc #$01
        sta $2007
        dex
        bne :-
        rts

.endproc
