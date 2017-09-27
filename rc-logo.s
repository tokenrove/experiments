        ;; Atari 2600 Recurse Center logo (or something resembling it...)
        ;; Julian Squires <julian@cipht.net> / 2017

        processor 6502
        include "vcs.h"
        include "macro.h"

        ;; these macros are from 8bitworkshop
        MAC TIMER_SETUP
.lines  SET {1}
        lda #(((.lines-1)*76-14)/64)
        sta WSYNC
        sta TIM64T
        ENDM

        MAC TIMER_WAIT
.waittimer
        lda INTIM
        bne .waittimer
        ENDM

        seg.u variables
        org $80

        seg code
        org $f000

start
        CLEAN_START

frame
        VERTICAL_SYNC

        TIMER_WAIT 37

        ;; colors!
        lda #$0e
        sta COLUBK
        lda #$c6                ; or maybe $c2, $c3, $d0
        sta COLUP0
        lda #$00
        sta COLUPF
        lda #%11001
        sta CTRLPF

        TIMER_WAIT              ; end of vbl

        ldx #37
.vblank sta WSYNC
        dex
        bne .vblank
; Disable VBLANK
        stx VBLANK

        ldy #7
        stx PF0
        stx PF1
        stx PF2
        sta WSYNC
        SLEEP 38
        sta RESP0
.initial_margin
        sta WSYNC
        dey
        bne .initial_margin

        ;; 16 times:
        ldy #16
next_line:
        ;; load next row of stuff
        lda pf0data,y
        sta PF0
        lda pf1data,y
        sta PF1
        lda pf2data,y
        sta PF2
        lda grp0data,y
        sta GRP0
        lda nusiz0data,y
        sta NUSIZ0
        ;; chill for 8 lines
        ldx #8
.1      sta WSYNC
        dex
        bne .1
        dey
        bne next_line

        ldy #56
        lda #$00
        sta PF0
        sta PF1
        sta PF2
.3
        sta WSYNC
        dey
        bne .3

        lda #2
        sta VBLANK

        ;; overscan
        TIMER_SETUP 29
        TIMER_WAIT
        jmp frame

        align $100
pf0data: .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
pf1data: .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
pf2data: .byte 0,$ff,$ab,$57,$fe,$f0,$ff,$01,$fd,$fd,$fd,$fd,$fd,$fd,$01,$ff
grp0data: .byte 0,0,0,0,0,0,0,0,0,0,  $ff, 0,  $f0, 0,0,0
nusiz0data: .byte 0,0,0,0,0,0,0,0,0,0, %1, 0, %011, 0,0,0

        org $fffc
        .word start
        .word start
