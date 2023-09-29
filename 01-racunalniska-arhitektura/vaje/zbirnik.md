# Zbirnik

Pri vseh nalogah se vam pod razdelkom **RAM** splača nastaviti _View_ na _Decimal_, da bodo vrednosti v pomnilniku predstavljene z običajnim desetiškim zapisom. Prav tako lahko pod možnostjo _Register addressing_ obkljukajte tiste registre, v katerih bodo shranjeni naslovi, saj bodo tisti naslovi ustrezno obarvani.

## Računanje ostanka

Zapišite program, ki v register `A` zapiše ostanek pri deljenju registra `A` z registrom `B`.

### Rešitev

    MOV C, A    ; naredi kopijo A v C
    DIV B       ; A celoštevilsko deli z B
    MUL B       ; A pomnoži z B (zdaj je v A razlika do ostanka deljenja)
    SUB C, A    ; od C (prvotni A) odsteje A (zdaj je v C iskani ostanek)
    MOV A, C    ; premakne ostanek v A
    HLT         ; prekinemo izvajanje

## Zaporedna števila

Zapišite program, ki na sklad zaporedno postavlja števila od 13 do 42.

### Rešitev

    MOV A, 13       ; v A bo shranjeno naslednje število za na sklad
    MOV B, 42       ; v B bo shranjeno zadnje število, ki ga bomo zapisali

    zanka:          ; sledeče ukaze bomo ponavljali, dokler A ne bo večji od B
        CMP A, B    ; primerjamo A in B
        JA konec    ; če je A večji od B, končamo, sicer nadaljujemo
        PUSH A      ; A shranimo na sklad
        INC A       ; A povečamo
        JMP zanka   ; skočimo na začetek zanke

    konec:
        HLT        ; prekinemo izvajanje

## Iskanje najmanjšega števila v seznamu

Zapišite program, ki poišče najmanjše število v danem seznamu. Seznam naj bo podan na začetku pomnilnika in sicer tako, da je na prvih dveh bajtih ukaz za skok na začetek programa, v tretjem bajtu dolžina seznama, v naslednjih bajtih zaporedoma elementi seznama, takoj za njimi pa bajt, v katerega naj se zapiše najmanjše število.

    JMP main
    dolzina:
        DB 10    ; število elementov v seznamu
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
        ...

### Rešitev

    JMP main
    dolzina:
        DB 10    ; število elementov v seznamu
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
        MOV A, seznam       ; register A bo kazal na trenutni element
                            ; na začetku torej na prvi element
        MOV B, [seznam]     ; register B bo hranil trenutni minimum
                            ; na začetku torej prvi element
        MOV C, seznam       ; register C bo kazal na konec seznama
        ADD C, [dolzina]    ; torej naslov zadnjega elementa + 1
    .zanka:
        INC A               ; premaknemo se na naslednji element
        CMP A, C            ; ali smo prišli do konca seznama?
        JE .konec           ; če smo, končamo zanko
        CMP B, [A]          ; ali smo našli manjši element?
        JBE .zanka          ; če nismo, gremo na naslednjega
        MOV B, [A]          ; sicer ga prej še zapišemo v B
        JMP .zanka          ; in gremo na naslednjega
    .konec:
        MOV [minimum], B    ; shranimo minimum
        HLT                 ; prekinemo izvajanje

## Indeks najmanjšega števila v seznamu

Zapišite funkcijo `poisci_minimum`, ki v register `B` shrani indeks najmanjšega števila v rezini `[A:C]` torej med števili, ki se nahajajo od vključno naslova, shranjenega v `A`, do naslova pred tistim, shranjenim v `C`. Funkcija naj z izjemo registra `B` vrednosti ostalih registrov pusti nespremenjene. Če funkcija deluje pravilno, bi moral spodnji program delovati kot tisti iz prejšnje naloge:

    main:
        ; pripravimo parametre funkcije
        MOV A, seznam
        MOV C, seznam
        ADD C, [dolzina]
        ; pokličemo funkcijo
        CALL poisci_minimum
        ; v mesto, na katerega kaže minimum, shranimo vrednost, na katero kaže B
        ; ker tega ne moremo narediti direktno, si pomagamo z registrom C
        PUSH C 
        MOV C, [B]
        MOV [minimum], C
        POP C
        HLT

### Rešitev

    poisci_minimum:
        PUSH A                ; shranimo staro vrednost registra A
        PUSH D                ; shranimo staro vrednost registra D
        MOV B, A              ; register B bo kazal na trenutni minimum
    .zanka_minimum:
        INC A                 ; premaknemo se na naslednji element
        CMP A, C              ; ali smo prišli do konca seznama?
        JE .konec_minumum     ; če smo, končamo zanko
        MOV D, [B]            ; da [A] primerjamo z [B], si [B] shranimo v D
        CMP D, [A]            ; ali smo našli manjši element?
        JBE .zanka_minimum    ; če nismo, gremo na naslednjega
        MOV B, A              ; sicer ga prej še zapišemo v B
        JMP .zanka_minimum    ; in gremo na naslednjega
    .konec_minumum:
        POP D
        POP A
        RET

## Urejanje seznama

Zapišite funkcijo `uredi`, ki elemente v rezini [A:C] uredi od najmanjšega do največjega. Pri tem naj vrednosti vseh registrov pusti pri miru. Eden najenostavnejših algoritmov za urejanje je urejanje z izbiranjem. V njem se zapeljete čez seznam, poiščete indeks najmanjšega elementa, nato pa ta element zamenjate s tistim na prvem mestu. Postopek nadaljujete s preostankom seznama, dokler ne pridete do konca.

Delovanje lahko preverite s sledečim programom:

    main:
                            ; pripravimo argumente za funkcijo uredi
        MOV A, seznam       ; register A mora kazati na prvi element
        MOV C, seznam       ; register C mora kazati na zadnji element + 1
        ADD C, [dolzina]
        CALL uredi          ; pokličemo funkcijo za urejanje
        HLT                 ; prekinemo izvajanje

### Rešitev

    uredi:
        ; na sklad shranimo vrednosti registrov, ki jih bomo povozili
        PUSH A
        PUSH B
        PUSH D
    .zanka_uredi:
        CMP A, C               ; smo dosegli konec tabele?
        JZ konec_uredi         ; če smo, končamo
        CALL poisci_minimum    ; sicer v B shranimo indeks minimuma
        MOV D, [A]             ; na sklad shranimo trenutni prvi element
        PUSH D                 ;
        MOV D, [B]             ; na njegovo mesto postavimo najmanjši element
        MOV [A], D
        POP D
        MOV [B], D             ; na mesto najmanjšega elementa pa damo prej prvega
        INC A                  ; pogledamo naslednji element
        JMP .zanka_uredi       ; in ponavljamo
    konec_uredi:
        ; s sklada povrnemo vrednosti spremenjenih registrov
        POP D
        POP B
        POP A
        RET
