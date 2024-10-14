JMP main

kopiraj:        ; v B kopiraj vrednost A
PUSH A          ; shrani staro vrednost A
CMP A, 0        ; če je A = 0
JE .konec       ; robni primer
DEC A
CALL kopiraj
INC B
JMP .epilog

.konec:
MOV B, 0

.epilog:
POP A           ; povrnemo vrednost A
RET             ; in se vrnemo nazaj na mesto klica

main:
MOV A, 42
CALL kopiraj
