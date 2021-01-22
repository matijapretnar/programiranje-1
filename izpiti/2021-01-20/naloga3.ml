(*============================================================================*]
 Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
 gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
 Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
 fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
 prve fotografije in lokacijo druge fotografije kar se da velik vzpon.

 Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
 vrstnem redu, kot si sledijo po poti. Na primer:

    [350; 230; 370; 920; 620; 80; 520; 780; 630]

 V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
 in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
 Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
 razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
[*============================================================================*)

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
  višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
  utemeljite v komentarju.
[*----------------------------------------------------------------------------*)


(* b *)(*----------------------------------------------------------------------------*]
  Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
  tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
[*----------------------------------------------------------------------------*)

