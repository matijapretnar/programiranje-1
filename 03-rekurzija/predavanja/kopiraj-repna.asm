JMP main

kopiraj:        ; v B kopiraj vrednost A
PUSH A          ; shrani staro vrednost A
MOV B, 0        ; B nastavi na 0
CALL pomozna    ; pokliči pomožno funkcijo
POP A           ; povrnemo vrednost A
RET             ; vrni rezultat

pomozna:        ; B prištej vrednost A, A se lahko pokvari
CMP A, 0        ; če je A = 0
JE .konec       ; robni primer
DEC A
INC B
JMP pomozna

.konec:

.epilog:
RET             ; in se vrnemo nazaj na mesto klica

main:
MOV A, 42
CALL kopiraj
