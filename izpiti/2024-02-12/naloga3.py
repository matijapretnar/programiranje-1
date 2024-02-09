# Ta je malo podobna : https://github.com/matijapretnar/programiranje-1/blob/master/izpiti/2021-08-24/izpit-2021-08-24.pdf ampak ni kritično

"""
Kurent mora po dolgi in naporni pustni soboti (zaradi kulturne izmenjave) obiskati še Ameriško mesto, ki ga predstavimo z matriko `N x M`.
Na vsakem mestu v matriki je celo (lahko negativno) število, ki predstavlja količino `bobov`.
Ker je Kurent, seveda ne sme iti kar tako, ampak mora upoštevati nekaj pravil:
- Pot po mreži se začne v zgornjem levem kotu in konča v spodnjem desnem kotu.
- Pot začne z 0 bobi in med potjo nikoli ne sme imeti negativnega števila bobov.
- Po mreži se lahko premika le en korak v desno, en krak navzdol ali en korak v desno in en korak navzdol (diagonalno) in za vsak tak korak plača 1 bob.
- Enkrat v sprehodu lahko skoči na poljubno mesto (tudi nazaj na mesto, kjer stoji) v mreži, vendar za tak skok plača 3 bobe.
- Ko se kurent nahaja na nekem mestu v mreži pobere vse bobe, ki so na tem mestu (če je število bobov negativno se to odšteje od skupnega števila bobov).
- Če se kurent večkrat znajde na istem mestu, bo vsakič pobral vse bobe, ki so na tem mestu (medtem, ko je bil odsoten se bobi obnovijo).

Pomagajte kurentu naiskati pot, ob kateri konča z največjim številom bobov.

"""