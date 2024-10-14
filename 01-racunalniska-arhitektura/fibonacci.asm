JMP main

; Funkcija, ki izračuna fib(A) in vrednost shrani v register A
fib:
    ; Shranimo vrednosti registrov
    PUSH C
    PUSH B

    ; V C shranimo začetno vrednost A
    MOV C, A

    ; Če je A = 0, je to tudi rezultat
    CMP A, 0
    JE .fib_end

    ; Če je A = 1, je to tudi rezultat
    CMP A, 1
    JE .fib_end

    ; V nasprotnem primeru najprej izračunamo fib(A - 1) in ga shranimo v B
    DEC C
    MOV A, C
    CALL fib
    MOV B, A

    ; Nato izračunamo še fib(A - 2) in ga shranimo v A
    DEC C
    MOV A, C
    CALL fib
    
    ; Nazadnje k A prištejemo še B, s čimer dobimo končni rezultat
    ADD A, B
    JMP .fib_end

.fib_end:
    ; Povrnemo vrednosti registrov in vrnemo rezultat
    POP B
    POP C
    RET

main:
    MOV B, 10
    MOV C, 20
    MOV D, 30
    MOV A, 8
    CALL fib
