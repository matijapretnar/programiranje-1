JMP main
dolzina:
    DB 10 ; st el. v sez.
seznam:
    DB 50    ; seznam
    DB 56
    DB 60
    DB 46
    DB 44
    DB 58
    DB 42
    DB 52
    DB 48
    DB 54
minimum:
    DB 0    ; na koncu bo tu minimum

main:
    MOV D, [dolzina]
    MOV C, seznam
    MOV A, 0
    MOV B, 255
    JMP for
    
for:
    JMP next_element
    here:
    CMP A, B
    JAE for
    MOV B, A
    JMP for

next_element:
    CMP D, 1
    JE end
    DEC D
    MOV A, [C]
    INC C
    JMP here
    
end:
    MOV [minimum], B
    HLT