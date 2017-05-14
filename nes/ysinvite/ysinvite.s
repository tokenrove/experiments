	;;
	;; HCVGS Ys III Competition Invitation
	;; Julian Squires <tek@wiw.org> / 2001
	;;

	processor 6502

	seg.u zeropage
	org $0000

screen:		ds.b 1
state:		ds.b 1
scrollx:	ds.b 1
tmp:		ds.b 3
sptimer:	ds.b 1

toad_x:		ds.b 1
toad_y:		ds.b 1
toad_frame:	ds.b 1
toad_framemask:	ds.b 1
toad_fcnt:	ds.b 1
toad_anim:	ds.w 1
toad_fspeed:	ds.b 1

head_x:		ds.b 1
head_y:		ds.b 1
head_frame:	ds.b 1
head_framemask:	ds.b 1
head_fcnt:	ds.b 1
head_anim:	ds.w 1
head_fspeed:	ds.b 1

music_pos_s0:	ds.b 1
music_lastblock_s0:	ds.b 1
music_lastlen_s0:	ds.b 1
music_fine_s0:	ds.b 1
music_song_s0:	ds.w 1
music_pos_s1:	ds.b 1
music_pos_tr:	ds.b 1
music_pos_no:	ds.b 1
music_fine_s1:	ds.b 1
music_fine_tr:	ds.b 1
music_fine_no:	ds.b 1

	seg.u data
	org $0400

sprtable:	ds.b 256

	seg text
	org $8000


	;; resethandler
	;; Called when the machine is reset.
	;; Never returns.
resethandler: subroutine
	sei
	cld

	;; make sure nmi is turned off
	lda #$00
	sta $2000

	;; wipe zero-page memory
	lda #$00
	ldx #$00
.loop:	sta $00,X
	inx
	bne .loop

	;; wipe sprite-table
	lda #$00
	ldx #$00
.loop2:	sta sprtable,X
	inx
	bne .loop2

	jmp main
	;; end of resethandler


	;; main
	;; This routine comprises the main body of the demo. It loads data
	;; for the appropriate screen into place, and makes sure that all the
	;; appropriate routines will be called.
	;; Called by reset.
main:	subroutine
	;; disable nmi on vblank, displays
	lda #%00001000
	sta $2000
	lda #%00000000
	sta $2001

	;; set screen/state to zero
	lda #$00
	sta screen
	sta state
	sta toad_frame
	lda #$10
	sta toad_x
	lda #$A0
	sta toad_y
	lda #$08
	sta toad_fspeed
	sta toad_fcnt
	lda #scr0_toad_walkright_framemask
	sta toad_framemask
	lda #>scr0_toad_walkright
	sta toad_anim+1
	lda #<scr0_toad_walkright
	sta toad_anim

	lda #$05
	sta sptimer

	lda #$00
	sta head_frame
	lda #$00
	sta head_x
	sta head_y
	lda #$08
	sta head_fspeed
	sta head_fcnt
	lda #scr0_head_invis_framemask
	sta head_framemask
	lda #>scr0_head_invis
	sta head_anim+1
	lda #<scr0_head_invis
	sta head_anim

	;; load screen zero palette, pattern table, name tables

	; palette
	lda #$3F
	sta $2006
	ldx #$00
	stx $2006
.pal_cp:
	lda scr0_palette,x
	sta $2007
	inx
	cpx #$20
	bne .pal_cp

	; pattern table 0
	lda #$00
	sta $2006
	sta $2006
	lda #>scr0_pattern
	ldx #>scr0_pattern_end
	jsr copy_to_vram

	; pattern table 1 (sprites)
	lda #$10
	sta $2006
	lda #$00
	sta $2006
	lda #>scr0_sprites
	ldx #>scr0_sprites_end
	jsr copy_to_vram

	; load name table 0 and attrib table 0
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	lda #>scr0_nam0
	ldx #>scr0_nam0_end
	jsr copy_to_vram

	; load name table 1 and attrib table 1
	lda #$24
	sta $2006
	lda #$00
	sta $2006
	lda #>scr0_nam1
	ldx #>scr0_nam1_end
	jsr copy_to_vram

	;; setup sprites, copy sprtable to spr-ram
	jsr load_toad
	jsr load_head

	;; init music routine
	jsr music_init

	;; enable display
	lda #%10100000
	sta $2000

	lda #%00011010
	sta $2001

	jmp .			; sit and spin.


	;; vblankhandler
	;; Called at each vblank.
vblankhandler: subroutine
	;; save registers
	pha
	txa
	pha
	tya
	pha

	lda #%00001000
	sta $2000
	lda #%00000000
	sta $2001

	;; switch screen
	lda screen
	bne .switch1
	jsr screen0_vbl
	jmp .end

.switch1:
	cmp #$01
	bne .switch2
	jsr screen1_vbl
	jmp .end

.switch2:
	cmp #$02
	bne .end
	jsr screen2_vbl

.end:	
	lda #>sprtable
	sta $4014

	;; update music
	jsr music_update

	;; enable display
	lda #%10100000
	sta $2000

	lda #%00011010
	sta $2001

	;; restore registers
	pla
	tay
	pla
	tax
	pla

	rti
	;; end of vblankhandler


	;; screen0_init
	;; sets up data for screen 0 - graphics, music, state
screen0_init: subroutine
	rts
	;; end of screen0_init


	;; screen0_vbl
	;; main screen 0 coordination routine, called every vblank
screen0_vbl: subroutine
	;; switch state
	lda state
	bne .switch1
	jsr scr0_scrollbg
	jmp .end

.switch1:
	cmp #$01
	bne .switch2
	jsr scr0_toadright
	jmp .end

.switch2:
	cmp #$02
	bne .switch3
	jsr scr0_toadpull
	jmp .end

.switch3:
	cmp #$03
	bne .switch4
	jsr scr0_fuckemup
	jmp .end

.switch4:
	cmp #$04
	bne .switch5
	jsr scr0_toadrun
	jmp .end

.switch5:
	jsr scr0_palfade

.end:
	;; update sprites
	dec toad_fcnt
	bne .headload
	lda toad_fspeed
	sta toad_fcnt
	inc toad_frame
	lda toad_frame
	and toad_framemask
	sta toad_frame

.headload:
	dec head_fcnt
	bne .doneload
	lda head_fspeed
	sta head_fcnt
	inc head_frame
	lda head_frame
	and head_framemask
	sta head_frame
.doneload:
	dec sptimer
	bne .noupdate
	lda #$03
	sta sptimer
	jsr load_toad
	jsr load_head
.noupdate:
	rts
	;; end of screen0_vbl


	;; scr0_scrollbg
	;; routine for screen 0, scrolls bg to the left
scr0_scrollbg: subroutine
	lda $2002
	lda scrollx
	sta $2005
	cmp #$FF
	bne .notdone
	inc state
.notdone:
	inc scrollx
	lda #$00
	sta $2005
	rts
	;; end of scr0_scrollbg


	;; scr0_toadright
	;; routine for screen 0, moves sprite to the right
scr0_toadright:	 subroutine
	inc toad_x
	lda toad_x
	cmp #$4F
	bne .end
	inc state
	lda #$00
	sta toad_frame
	lda #scr0_toad_pull_framemask
	sta toad_framemask
	lda #<scr0_toad_pull
	sta toad_anim
	lda #>scr0_toad_pull
	sta toad_anim+1
.end:	rts
	;; end of scr0_toadright


	;; scr0_toadpull
	;; routine for screen 0, makes toad pull up vegetable
scr0_toadpull:	subroutine
	; check the current frame (if the head is invisible)...
	lda head_x
	cmp #$00
	bne .pullhead

	; make the zombie head appear after first five frames
	lda toad_frame
	cmp #$05
	bmi .end
	lda #$00
	sta head_frame
	lda toad_x
	sbc #$04
	sta head_x
	lda toad_y
	adc #$18
	sta head_y
	lda #scr0_head_pull_framemask
	sta head_framemask
	lda #<scr0_head_pull
	sta head_anim
	lda #>scr0_head_pull
	sta head_anim+1
	jmp .end

	; pull the head up
.pullhead:
	lda toad_frame
	cmp #$03
	bmi .end
	lda sptimer
	cmp #$01
	bne .end
	dec head_y
	dec toad_fcnt

	; if it's free, switch into the ``fuck em up'' state
	lda head_y
	cmp toad_y
	bne .end

	inc state
	lda #$00
	sta toad_frame
	lda #$01
	sta toad_fspeed
	lda toad_fspeed
	sta toad_fcnt
	lda #scr0_toad_screamright_framemask
	sta toad_framemask
	lda #<scr0_toad_screamright
	sta toad_anim
	lda #>scr0_toad_screamright
	sta toad_anim+1

.end:	
	rts
	;; end of scr0_toadpull


	;; scr0_fuckemup
	;; routine for screen 0, makes the head fly around and toad run
	;; away screaming
scr0_fuckemup:	subroutine
	; head swoops up, then switches to his yelling graphic, and we
	; let loose with the samples

	; toad first jumps in the air in fright
	; is he finished? if so, on to the next state.
	inc state
	rts
	;; end of scr0_fuckemup


	;; scr0_toadrun
	;; routine for screen 0, makes the head fly around and toad run
	;; away screaming
scr0_toadrun:	subroutine
	; head flies in spirals
	inc head_x
	ldx head_x
	lda sinetable,x
	sta head_y
	inc toad_x
	; if toad's off the screen, move to the final state
	lda toad_x
	cmp #$FF
	bne .end
	inc state
.end:
	rts
	;; end of scr0_toadrun

	
	;; scr0_palfade
	;; Fade out the palette for screen 0
scr0_palfade: subroutine
	inc head_x
	ldx head_x
	lda sinetable,x
	sta head_y
	;; fade down the palette
	lda #<null_palette
	sta tmp
	lda #>null_palette
	sta tmp+1
	jsr fade_to_palette
	bne .end
	;;  if it's done, load screen 1.
	jsr screen1_init
.end:
	lda #$00
	sta $2006
	sta $2006
	rts
	;; end of scr0_palfade


	;; screen1_init
	;; loads data for screen 1 - graphics, music, state
screen1_init: subroutine
	lda #$01
	sta screen
	lda #$00
	sta state
	lda #$00
	sta $2005
	sta $2005
	rts
	;; end of screen1_init


	;; screen1_vbl
	;; coordination routine for screen 1
screen1_vbl: subroutine
	;; switch state
	lda state
	bne .switch1
	jsr scr1_fadein
	jmp .end

.switch1:
.end:
	rts
	;; end of screen1_vbl


	;; scr1_fadein
	;; Fades in the palette for screen 1
scr1_fadein: subroutine
	lda #<scr0_palette
	sta tmp
	lda #>scr0_palette
	sta tmp+1
;	jsr fade_to_palette
	bne .end
	; if it's finished, increment state
	inc state
.end:
	rts
	;; end of scr1_fadein


	;; screen2_vbl
	;; coordination routine for screen 2
screen2_vbl: subroutine
	rts
	;; end of screen2_vbl


	;;
	;; Utility functions follow.
	;;

	;; waitvbl
	;; Waits for VBlank.
waitvbl lda $2002
	bpl waitvbl
	rts


	;; copy_to_vram
	;; Copies full pages from a*$100 to vram, until x*$100
	;; Assumes the PPU address has already been written.
copy_to_vram: subroutine
	sta tmp+1
	lda #$00
	sta tmp
.outer_loop:
	ldy #$00
.inner_loop:
	lda (tmp),y
	sta $2007
	iny
	cpy #$00
	bne .inner_loop
	inc tmp+1
	txa
	cmp tmp+1
	bne .outer_loop
	rts
	;; end of copy_to_vram


	;; fade_to_palette
	;; Fades the current palette to a specified palette in memory.
fade_to_palette: subroutine
	; fade down each value
	ldy #$20
	lda #$00
	sta tmp+2
	lda $2002

.paldec:
	dey
	beq .palend

	lda #$3F
	sta $2006
	tya
	sta $2006
	lda $2007
	cmp (tmp),y
	beq .nosub
	bmi .add

	sbc #$01
	tax
	lda #$3F
	sta $2006
	tya
	sta $2006
	txa
	sta $2007
	jmp .paldec

.add:	adc #$01
	tax
	lda #$3F
	sta $2006
	tya
	sta $2006
	txa
	sta $2007
	jmp .paldec

.nosub: inc tmp+2
	jmp .paldec
.palend:

	; if they're all set, return 0.
	lda tmp+2
	cmp #$1F
	rts
	;; end of fadetopal


	;; music_init
	;; Initializes the music playback
music_init: subroutine
	lda #$00
	sta music_pos_s0		; square
	sta music_pos_s1		; square
	sta music_pos_tr		; triangle
	sta music_pos_no		; noise
	sta music_fine_s0		; square
	sta music_fine_s1		; square
	sta music_fine_tr		; triangle
	sta music_fine_no		; noise

	lda #<scr0_music_s0
	sta music_song_s0
	lda #>scr0_music_s0
	sta music_song_s0+1

	lda #scr0_music_s0_lastblock
	sta music_lastblock_s0
	lda #scr0_music_s0_lastlen
	sta music_lastlen_s0

	; enable all four channels
	lda #$0F
	sta $4015
	; play the first note
	jsr music_playnext_s0
	jsr music_playnext_s1
	jsr music_playnext_tr
	jsr music_playnext_no
	rts
	;; end of music_init


	;; music_playnext_s0
	;; Plays the next note for the first square channel (Gtr1)
music_playnext_s0: subroutine
	;; square zero
	lda music_pos_s0
	tay

	lda (music_song_s0),y
	sta $4000
	iny
	lda (music_song_s0),y
	sta $4001
	iny
	lda (music_song_s0),y
	sta $4002
	iny
	lda (music_song_s0),y
	sta $4003
	; Figure out how many frames long this note is.
	lsr
	lsr
	lsr
	tax
	lda music_timetable,x
	sta music_fine_s0

	lda music_pos_s0
	adc #$04
	sta music_pos_s0
	bne .noinc

	inc music_song_s0+1
.noinc
	lda music_song_s0+1
	cmp music_lastblock_s0
	bne .nocmp

	lda music_pos_s0
	cmp music_lastlen_s0
	bne .nocmp
	lda #$00
	sta music_pos_s0
	lda #>scr0_music_s0
	sta music_song_s0+1
.nocmp
	rts
	;; end of music_playnext_s0


	;; music_playnext_s1
music_playnext_s1: subroutine
	;; square one
	lda music_pos_s1
	asl
	asl
	tax
	inc music_pos_s1
	lda music_pos_s1
	cmp #scr0_music_s1len
	bmi .noinc
	; loop
	lda #$00
	sta music_pos_s1
.noinc
	lda scr0_music_s1,x
	sta $4004
	inx
	lda scr0_music_s1,x
	sta $4005
	inx
	lda scr0_music_s1,x
	sta $4006
	inx
	lda scr0_music_s1,x
	sta $4007
	; Figure out how many frames long this note is.
	lsr
	lsr
	lsr
	tax
	lda music_timetable,x
	sta music_fine_s1
	rts
	;; end of music_playnext_s1


	;; music_playnext_tr
music_playnext_tr: subroutine
	;; triangle
	lda music_pos_tr
	asl
	adc music_pos_tr
	tax
	inc music_pos_tr
	lda music_pos_tr
	cmp #scr0_music_trlen
	bmi .noinc
	; loop
	lda #$00
	sta music_pos_tr
.noinc
	lda scr0_music_tr,x
	sta $4008
	inx
	lda scr0_music_tr,x
	sta $400A
	inx
	lda scr0_music_tr,x
	sta $400B
	; Figure out how many frames long this note is.
	lsr
	lsr
	lsr
	tax
	lda music_timetable,x
	sta music_fine_tr
	rts
	;; end of music_playnext_tr


	;; music_playnext_no
music_playnext_no: subroutine
	;; noise
	lda music_pos_no
	asl			; shift-left + original == multiply-by-3
	adc music_pos_no
	tax
	inc music_pos_no
	lda music_pos_no
	cmp #scr0_music_nolen
	bmi .noinc
	; loop
	lda #$00
	sta music_pos_no
.noinc
	lda scr0_music_no,x
	sta $400C
	inx
	lda scr0_music_no,x
	sta $400E
	inx
	lda scr0_music_no,x
	sta $400F
	; Figure out how many frames long this note is.
	lsr
	lsr
	lsr
	tax
	lda music_timetable,x
	sta music_fine_no
	rts
	;; end of music_playnext_no


	;; music_update
music_update: subroutine
	dec music_fine_s0
	bne .nos0update
	jsr music_playnext_s0
.nos0update
	dec music_fine_s1
	bne .nos1update
	jsr music_playnext_s1
.nos1update
	dec music_fine_tr
	bne .notrupdate
	jsr music_playnext_tr
.notrupdate
	dec music_fine_no
	bne .nonoupdate
	jsr music_playnext_no
.nonoupdate
	rts
	;; end of music_update


	;; load_toad
load_toad: subroutine
	lda toad_frame
	asl
	asl
	asl
	asl
	tay
	ldx #$18
.sprload:
	; Y-coordinate
	lda (toad_anim),y
	adc toad_y
	sta sprtable,x
	inx
	iny
	; Tile index
	lda (toad_anim),y
	sta sprtable,x
	inx
	iny
	; Attributes
	lda (toad_anim),y
	sta sprtable,x
	inx
	iny
	; X-coordinate
	lda (toad_anim),y
	adc toad_x
	sta sprtable,x
	inx
	iny

	cpx #$28
	bne .sprload
	rts
	;; end of load_toad


	;; load_head
load_head: subroutine
	lda head_frame
	asl
	asl
	asl
	asl
	adc head_frame
	adc head_frame
	adc head_frame
	adc head_frame
	adc head_frame
	adc head_frame
	adc head_frame
	adc head_frame
	tay
	ldx #$00
.sprload:
	; Y-coordinate
	lda (head_anim),y
	adc head_y
	sta sprtable,x
	inx
	iny
	; Tile index
	lda (head_anim),y
	sta sprtable,x
	inx
	iny
	; Attributes
	lda (head_anim),y
	sta sprtable,x
	inx
	iny
	; X-coordinate
	lda (head_anim),y
	adc head_x
	sta sprtable,x
	inx
	iny

	cpx #$18
	bne .sprload
	rts
	;; end of load_head


scr0_palette:	incbin screen0.pal
null_palette:	ds.b $20,$0e
	align $0100
scr0_pattern:	incbin screen0.ptable
scr0_pattern_end:
scr0_sprites:	incbin screen0.spr
scr0_sprites_end:
scr0_nam0:	incbin screen0.nam0
scr0_nam0_end:
scr0_nam1:	incbin screen0.nam1
scr0_nam1_end:

scr0_toad_walkleft:
	dc.b $00,$C1,$00,$00, $00,$C3,$00,$08, $10,$C5,$00,$00, $10,$C7,$00,$08
	dc.b $00,$C1,$00,$00, $00,$C3,$00,$08, $10,$C9,$00,$00, $10,$CB,$00,$08
	dc.b $00,$C3,$00,$00, $00,$C1,$00,$08, $10,$C7,$00,$00, $10,$C5,$00,$08
	dc.b $00,$C1,$00,$00, $00,$C3,$00,$08, $10,$C9,$00,$00, $10,$CB,$00,$08
scr0_toad_walkleft_nframes equ 3
scr0_toad_walkleft_framemask equ 3

scr0_toad_walkright:
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$C7,$40,$00, $10,$C5,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$C7,$40,$00, $10,$C5,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08
scr0_toad_walkright_nframes equ 3
scr0_toad_walkright_framemask equ 3

scr0_toad_pull:
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$E1,$00,$00, $00,$E3,$00,$08, $10,$E5,$00,$00, $10,$E7,$00,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
	dc.b $00,$BB,$00,$00, $00,$BB,$40,$08, $10,$EF,$00,$00, $10,$EF,$40,$08
scr0_toad_pull_nframes equ 1
scr0_toad_pull_framemask equ $0f

scr0_toad_screamright:
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$C7,$40,$00, $10,$C5,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$C7,$40,$00, $10,$C5,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08

	dc.b $00,$CF,$40,$00, $00,$CD,$40,$08, $10,$D3,$40,$00, $10,$D1,$40,$08
	dc.b $00,$CF,$40,$00, $00,$CD,$40,$08, $10,$D3,$40,$00, $10,$D1,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08
	dc.b $00,$C3,$40,$00, $00,$C1,$40,$08, $10,$CB,$40,$00, $10,$C9,$40,$08
scr0_toad_screamright_nframes equ 3
scr0_toad_screamright_framemask equ 7

scr0_head_invis:
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
	dc.b $00,$00,$00,$00, $00,$00,$00,$00, $00,$00,$00,$00
scr0_head_invis_framemask equ 0

scr0_head_pull:
	dc.b $00,$01,$01,$00, $00,$03,$01,$08, $00,$05,$01,$10
	dc.b $10,$07,$01,$00, $10,$09,$01,$08, $10,$0B,$01,$10

	dc.b $00,$01,$01,$00, $00,$0D,$01,$08, $00,$0F,$01,$10
	dc.b $10,$07,$01,$00, $10,$09,$01,$08, $10,$0B,$01,$10
scr0_head_pull_framemask equ 1

	include screen0.music.s

music_timetable: dc.b $05,$7F,$0A,$01,$14,$02,$28,$03
	         dc.b $50,$04,$1E,$05,$07,$06,$0E,$07
		 dc.b $06,$08,$0C,$09,$18,$0A,$30,$0B
	         dc.b $60,$0C,$24,$0D,$08,$0E,$10,$0F

	align $100
sinetable: dc.b 32, 32,
        dc.b 33, 34, 35, 35, 36, 37, 38, 39, 39, 40, 41, 42, 42, 43, 44,
        dc.b 44, 45, 46, 47, 47, 48, 49, 49, 50, 51, 51, 52, 52, 53, 54,
        dc.b 54, 55, 55, 56, 56, 57, 57, 58, 58, 59, 59, 59, 60, 60, 60,
        dc.b 61, 61, 61, 62, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63,
        dc.b 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 62, 62,
        dc.b 62, 62, 61, 61, 61, 60, 60, 60, 59, 59, 59, 58, 58, 57, 57,
        dc.b 56, 56, 55, 55, 54, 54, 53, 52, 52, 51, 51, 50, 49, 49, 48,
        dc.b 47, 47, 46, 45, 44, 44, 43, 42, 42, 41, 40, 39, 39, 38, 37,
        dc.b 36, 35, 35, 34, 33, 32, 31, 31, 30, 29, 28, 28, 27, 26, 25,
        dc.b 24, 24, 23, 22, 21, 21, 20, 19, 19, 18, 17, 16, 16, 15, 14,
        dc.b 14, 13, 12, 12, 11, 11, 10, 9, 9, 8, 8, 7, 7, 6, 6,
        dc.b 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1,
        dc.b 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        dc.b 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3,
        dc.b 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10,
        dc.b 11, 11, 12, 12, 13, 14, 14, 15, 16, 16, 17, 18, 19, 19, 20,
        dc.b 21, 21, 22, 23, 24, 24, 25, 26, 27, 28, 28, 29, 30, 31

	;; Interrupt Vectors
	ds $fffa-.

nmivec	dc.w vblankhandler	; NMI routine
rstvec	dc.w resethandler	; Reset routine
irqvec	dc.w resethandler	; BRK routine

	;; EOF ysinvite.s