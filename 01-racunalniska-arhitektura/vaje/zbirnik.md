# Zbirnik

Pri vseh nalogah se vam pod razdelkom **RAM** splača nastaviti _View_ na _Decimal_, da bodo vrednosti v pomnilniku predstavljene z običajnim desetiškim zapisom. Prav tako lahko pod možnostjo _Register addressing_ obkljukajte tiste registre, v katerih bodo shranjeni naslovi, saj bodo tisti naslovi ustrezno obarvani.

## Računanje ostanka

Zapišite program, ki v register `A` zapiše ostanek pri deljenju registra `A` z registrom `B`.

## Zaporedna števila

Zapišite program, ki na sklad zaporedno postavlja števila od 13 do 42.

## Menjava pomnilniških celic

Poleg registrov in sklada lahko podatke shranjujemo tudi v pomnilniške celice. To storimo prek naslovov oblike `[N]`, kjer je `N` zaporedni indeks mesta v pomnilniku, ali `[R]`, kjer je `R` ime registra, v katerem je shranjen indeks. Zapišite program, ki zamenja vsebini pomnilniških celic na naslovih `[A]` in `[B]`.

## Iskanje najmanjšega števila v seznamu

Zapišite program, ki poišče najmanjše število v danem seznamu. Seznam naj bo podan na začetku pomnilnika in sicer tako, da je v prvih dveh celicah ukaz za skok na začetek programa, v tretji celici dolžina seznama, v naslednjih celicah zaporedoma elementi seznama, takoj za njimi pa celica, v katerega naj se zapiše najmanjše število. Podatke lahko v pomnilnik zapišemo z ukazom `DB`, ki se ne prevede v noben strojni ukaz, ampak samo shrani vrednost v naslednji prostor v pomnilniku. Zgornje podatke bi z njim zapisali takole:

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

## Izračun praštevil

Če vam ostaja čas, poskusite napisati program, ki čim bolj učinkovito računa zaporedna praštevila in jih dodaja na sklad. Programe bomo primerjali po hitrosti in porabljenem prostoru, preizkusili pa jih bomo na simulatorju, ki podpira neomejenim skladom ter števila, večja od 255.
