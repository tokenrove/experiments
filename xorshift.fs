\ xorshift64*
\ assumes a 64-bit forth like gforth on x86-64

variable xorshift-state
: seed-from-time
  utime xor ?dup 0= if 1 then
  xorshift-state ! ;
seed-from-time

: xorshift64* ( -- u )
  xorshift-state @
  dup 12 rshift xor
  dup 25 lshift xor
  dup 27 rshift xor
  dup xorshift-state !
  2685821657736338717 * ;

: choose ( n -- u ) xorshift64* um* swap drop ;
