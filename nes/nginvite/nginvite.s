;
; Ninja Gaiden Invitation
;

	org $C000

;
; reset, called on boot
;
reset	cld
	sei
	ldx #$ff
	txs

	lda #$00
	sta $00
	sta $01
	sta $02
	sta $03
	sta $04

	lda #$ee
	sta $01

	jsr initppu
	cli
	jmp main

;
; nmi [every retrace]
;
nmi	sei

	pha
	txa
	pha
	tya
	pha
	jsr shutdownvid

	jsr drawmsg

	lda $04
	and #$01
	cmp #$00
	beq check1
	; if bit 1 is set, do palette tricks
	lda #$3F
	sta $2006
	lda #$00
	sta $2006

	lda $03
	sta $2007
	sta $2007
	sta $2007
	lda $02
	sta $2007

check1
	; handle scrolling
	; note that this _must_ be the last ppu-related write before
	; we enable the video/end the nmi routine
	lda $02
	sta $2005
	lda #$00
	sta $2005
	inc $02
	lda #$08
	cmp $02
	bne notyet
	inc $03
	lda #$00
	sta $02
	lda $03
	tay
	lda ($00), y
	cmp #$00
	bne notyet
	lda #$00
	sta $03

notyet	jsr initvid
	pla
	tay
	pla
	tax
	pla

	cli
	rti

;
; irq
;
irq	rti

main	; loop forever
l001	jsr waitvbl
	lda $04
	and #$02
	cmp #$00
	beq check2

	; play fairly bad sounding sound effect
	lda #$01
	sta $4015
	lda #$E9
	sta $4000
	lda #$77
	sta $4001
	lda #$E0
	sta $4002
	lda #$37
	sta $4003
	lda $04
	and #$FD
	sta $04

check2
	jmp l001

;
; Set up the PPU
;
initppu	jsr waitvbl

	jsr shutdownvid
	jsr loadvrom

	jsr initpalette
	jsr blankscr

	jsr initvid
	rts

;
; disable display while we do things
;
shutdownvid
	lda #$00
	sta $2000
	sta $2001
	rts

;
; start displaying things
;
initvid
	lda #$88
	sta $2000
	lda #$5E
	sta $2001
	rts

;
; wipe the screen (with spaces [0x20])
;
blankscr
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	lda #$20
	ldx #$80
blank01	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	dex
	bne blank01
	rts

;
; setup the palettes (assumes you're calling within the vblank, with
; display turned off)
;
initpalette
	; set bg palette
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	tax

l1	lda bgpalette, x
	sta $2007
	inx
	cpx #$10
	bne l1
	rts

;
; puts the c-style message (pointer at $00-$01)
; Needs some restructuring; documentation
;
drawmsg
	lda #$20
	sta $2006
	sta $2006
	; $03 contains the offset into the string thus far
	lda $03
	tay
	ldx #$00
drawmsgloop
	lda ($00), y
	cmp #$00
	bne drawmsgl2
	; we hit the end - loop from the beginning
	lda $00
	tay
	lda ($00), y

drawmsgl2
	cmp #$0a
	bne drawmsgl3
	; otherwise, toggle l33t palette effects
	lda $04
	eor #$01
	sta $04
	lda #$0c
	sta ($00), y
	dex	; dex because this isn't a ``real'' character
	iny
	lda ($00), y

drawmsgl3
	cmp #$0b
	bne drawmsgl4
	; play absurdly sinister sound effect
	lda $04
	ora #$02
	sta $04
	lda #$0c
	sta ($00), y
	dex
	iny
	lda ($00), y
drawmsgl4
	iny
	inx
	cpx #$21
	beq drawmsgend
	sta $2007
	jmp drawmsgloop
drawmsgend
	lda #$20
	sta $2007
	rts

;
; load vromdata into the PPU VRAM (pattern tables, sprites init)
;
loadvrom
	rts

;
; wait for vblank
;
waitvbl	lda $2002
	bpl waitvbl
	rts

bgpalette
	db $0d
	db $00
	db $00
	db $12
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00
	db $00

	pad $ee00
cmsg	db $0b,"                                "
	db "Retsyn -- this is your official invitation to "
	db "the 7null9 Ninja Gaiden Tournament. January 13th"
	db $2c," 2001 - all day. And see you next! -- tek."

	db $20
	db $20
	db $20
	db $20
	db $20
	db $2e
	db $2e
	db $2e
	db $20
	db $61
	db $6E
	db $64
	db $20
	db $6E
	db $6F
	db $77
	db $2c
	db $20
	db $6c
	db $65
	db $74
	db $27
	db $73
	db $20
	db $0a
	db $0b
	db $68
	db $61
	db $76
	db $65
	db $20
	db $61
	db $20
	db $73
	db $65
	db $69
	db $7A
	db $75
	db $72
	db $65
	db $21

	db $00

	pad $fffa

nmivec	dw nmi
rstvec	dw reset
irqvec	dw irq

; EOF nginvite.s
