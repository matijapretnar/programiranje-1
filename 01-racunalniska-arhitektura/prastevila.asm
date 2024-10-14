PUSH 2
PUSH 3
MOV C, 3

naslednje_stevilo:
ADD C, 2

MOV D, 230

naslednji_delitelj:
MOV A, [D]
MUL A
CMP A, C
JA je_prastevilo

MOV A, C
DIV [D]
MUL [D]
CMP A, C
JE naslednje_stevilo

DEC D

JMP naslednji_delitelj

je_prastevilo:
PUSH C
JMP naslednje_stevilo
