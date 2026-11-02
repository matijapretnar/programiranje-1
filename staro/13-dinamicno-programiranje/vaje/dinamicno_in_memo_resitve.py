from functools import cache
# =============================================================================
# Najdaljše naraščajoče podzaporedje
# =============================================================================

# -----------------------------------------------------------------------------
# Napišite funkcijo `najdaljse_narascajoce_podazporedje`, ki sprejme seznam in
# poišče najdaljše (ne strogo) naraščajoce podzaporedje števil v seznamu.
#
# Primer: v seznamu `[2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]` kot rezultat vrne
# podzaporedje `[2, 3, 4, 4, 6, 7, 8, 9]`.
# -----------------------------------------------------------------------------
def najdaljse_narascajoce_podazporedje(sez):
    n = len(sez)
    naj_podseznam_z_zacetkom = [[] for _ in range(n)] # shranjuje narascajoce podzaporedje od i-tega elementa naprej, ki se zacne s sez[i]
    koncni_rezultat = []

    for i in range(n - 1, -1, -1):
        naj_podseznam_z_zacetkom[i] = [sez[i]]
        for j in range(i + 1, n):
            if sez[j] >= sez[i] and len(naj_podseznam_z_zacetkom[j]) + 1 > len(naj_podseznam_z_zacetkom[i]):
                naj_podseznam_z_zacetkom[i] = [sez[i]] + naj_podseznam_z_zacetkom[j]
     
        if len(naj_podseznam_z_zacetkom[i]) > len(koncni_rezultat):
            koncni_rezultat = naj_podseznam_z_zacetkom[i]
    
    return koncni_rezultat

def cache_najdaljse_narascajoce_podzaporedje(sez):
    n = len(sez)

    @cache
    def pomozna(i): # kot zgoraj vrne najdaljse narascajoce podzaporedje od i-tega elementa naprej, ki se zacne s sez[i]
        if i == n:
            return []
        trenutni = [sez[i]]
        for j in range(i + 1, n):
            if sez[j] >= sez[i]:
                kandidat = [sez[i]] + pomozna(j)
                if len(kandidat) > len(trenutni):
                    trenutni = kandidat
        return trenutni

    najdaljsi = []
    for i in range(n):
        kandidat = pomozna(i)
        if len(kandidat) > len(najdaljsi):
            najdaljsi = kandidat
    return najdaljsi
    
print(najdaljse_narascajoce_podazporedje([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]))
print(najdaljse_narascajoce_podazporedje([9, 7, 8, 9, 1, 2, 3]))
print(cache_najdaljse_narascajoce_podzaporedje([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]))
print(cache_najdaljse_narascajoce_podzaporedje([9, 7, 8, 9, 1, 2, 3]))
# -----------------------------------------------------------------------------
# Rešitev sedaj popravite tako, da funkcija `vsa_najdaljsa` vrne seznam vseh
# najdaljših naraščajočih podzaporedij.
# -----------------------------------------------------------------------------
def vsa_najdaljsa(sez):
    n = len(sez)
    pomozni_r = [[] for _ in range(n)]
    najdaljsa_podzap = [[]]

    for i in range(n - 1, -1, -1):
        trenutni = [sez[i]]
        for j in range(i + 1, n):
            if sez[j] >= sez[i] and len(pomozni_r[j]) + 1 > len(trenutni):
                trenutni = [sez[i]] + pomozni_r[j]
        pomozni_r[i] = trenutni

        if len(trenutni) > len(najdaljsa_podzap[0]):
            najdaljsa_podzap = [trenutni]
        elif len(trenutni) == len(najdaljsa_podzap[0]):
            najdaljsa_podzap.append(trenutni)  
    return najdaljsa_podzap

print(vsa_najdaljsa([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]))
print(vsa_najdaljsa([9, 7, 8, 9, 1, 2, 3]))

# =============================================================================
# Žabica
# =============================================================================
# Žabica se je izgubila v močvari in želi kar se da hitro odskakljati ven. Na
# srečo močvara vsebuje veliko muh, s katerimi si lahko povrne energijo, kajti
# utrujena žabica ne skoči daleč.
# 
# S funkcijo `zabica(mocvara)` želimo ugotoviti, kako hitro lahko žabica
# odskaklja iz močvare. Močvaro predstavimo s tabelo, kjer žabica prične na
# ničtem polju. Če je močvara dolžine `k`, je cilj žabice priskakljati vsaj na
# `k`-to polje ali dlje (torej prvo polje, ki ni več vsebovano v tabeli).
# 
# Energičnost žabice predstavimo z dolžino najdaljšega možnega skoka. Torej
# lahko žabica z količino energije `e` skoči naprej za katerokoli razdaljo med
# `1` in `e`, in če skoči naprej za `k` mest ima sedaj zgolj `e - k` energije.
# Na vsakem polju močvare prav tako označimo, koliko energije si žabica povrne,
# ko pristane na polju. Tako se včasih žabici splača skočiti manj daleč, da
# pristane na polju z več muhami. Predpostavimo, da ima vsako polje vrednost
# vsaj `1`, da lahko žabica v vsakem primeru skoči naprej.
# 
# V primeru `[2, 4, 1, 2, 1, 3, 1, 1, 5]` lahko žabica odskaklja iz močvare v
# treh skokih, v močvari `[4, 1, 8, 2, 11, 1, 1, 1, 1, 1]` pa potrebuje zgolj
# dva.
# =============================================================================
def cache_zabica(mocvara):
    n = len(mocvara)

    @cache
    def pomozna(pozicija, energija):
        # robni primer - skocimo lahko do konca
        if pozicija + energija >= n:
            return 1
        
        najmanj_skokov = n
        for skok in range(1, energija + 1):
            nova_pozicija = pozicija + skok
            nova_energija = energija - skok + mocvara[nova_pozicija]

            skoki = pomozna(nova_pozicija, nova_energija) + 1
            najmanj_skokov = min(najmanj_skokov, skoki)
        
        return najmanj_skokov

    return pomozna(0, mocvara[0])

print(cache_zabica([2, 4, 1, 2, 1, 3, 1, 1, 5]))
print(cache_zabica([4, 1, 8, 2, 11, 1, 1, 1, 1, 1]))

# =============================================================================
# Nageljni
# =============================================================================
# Mama Franca želijo na balkon širine `n` postaviti `m` korit z nageljni širine
# `l` (korit, ne nageljnov). Zaradi lažjega zalivanja mora biti med dvema
# koritoma vsaj za 1 enoto prostora. Mama Franca želijo postaviti vsa korita,
# jih pa zaradi slabega vida med seboj ne razlikujejo. 
# 
# Vnuk je že spisal program, ki poišče število možnih postavitev, ne zna pa
# vrniti rešitev. Napišite funkcijo `nageljni(n, m, l)`, ki vrne seznam vseh
# možnih postavitev, da se bodo mama Franca lažje odločili.
# 
# Primer vseh štirih možnih postavitev pri balkonu širine 9 s tremi koriti
# širine 2 (kjer z 1 označimo nagelj in z 0 prazen prostor):
# 
#     [1, 1, 0, 1, 1, 0, 1, 1, 0]
#     [1, 1, 0, 1, 1, 0, 0, 1, 1]
#     [1, 1, 0, 0, 1, 1, 0, 1, 1]
#     [0, 1, 1, 0, 1, 1, 0, 1, 1]
# =============================================================================
@cache
def nageljni(n, m, l):
    # robni primeri
    # negativni primer - ni prostora
    if n < m * l + (m - 1):
        return []
    
    # pozitivni primeri
    # 1. postavili vsa korita
    if m == 0:
        return [[0] * n]
    # 2. eno korito, ki zapolni ves prostor (to je primer, ki ga rekurzivni klici ne pokrijejo samodejno)
    if m == 1 and n == l: 
        return [[1] * n]
    # --- tu bi lahko napisali robni primer za vse moznosti "m == 1", a so pokriti spodaj ---

    # rekurzivni klici
    postavitve = []

    for postavitev in nageljni(n - l - 1, m - 1, l):
        nova_postavitev = [1] * l + [0] + postavitev
        postavitve.append(nova_postavitev)

    for postavitev in nageljni(n - 1, m, l):
        nova_postavitev = [0] + postavitev
        postavitve.append(nova_postavitev)

    return postavitve

print(nageljni(9, 3, 2))


# =============================================================================
# Pobeg iz Finske
# =============================================================================
# Vaš sošolec Mortimer se je med potovanjem po Finski spravil v krepko godljo.
# Po divjem poskušanju lokalne vodke se je namreč stepel s kravo, zaradi česar
# ga sedaj lovi finska govedorejska mafija. Na srečo so za njegovo hrabro bitko
# slišale vse rokovske in metalske skupine, ki so mu pripravljene ponuditi
# prevoz.
# 
# Ker je Mortimer pridno poslušal predavanja iz finančne matematike, med potjo
# uspe prislužiti nekaj denarja, s katerim bo lahko plačal prevoz. Finci,
# navdušeni nad Mortimerjevim pogumom, mu dovolijo, da se med potjo zadolži,
# dokler na koncu pobega vse stroške povrne.
# 
# Mesta na poti predstavimo kot seznam, katerega elementi so seznami vseh
# možnih nadaljnjih poti. Pot je par `(indeks_cilja, denar)`. Kot primer
# 
#     [[(1, 10), (3, -10)],    # 0 
#     [(2, 10), (5, -20)],     # 1
#     [(3, -10)],              # 2 
#     [(4, 15)],               # 3 
#     [(5, 0)]]                # 4 
# 
# pomeni, da lahko v mestu 1 Mortimer izbere med prevozom v mesto 2, kjer
# dodatno zasluži 10 evrov, ali pa prevoz v mesto 5, ki ga stane 20 evrov. Ker
# beži pred mafijo, lahko predpostavite, da bodo možne zgolj poti na mesta z
# višji indeksom (torej ni ciklov).
# 
# Pobeg je uspešen, čim lahko odpotuje v mesto, ki ni več na seznamu (torej
# skok na indeks, ki preseže seznam) in ima po koncu zadnjega skoka 0 ali več
# evrov. Napišite program, ki nam vrne pot z najmanjšim številom skokov,
# predstavljeno kot seznam indeksov mest na poti. Ker pobeg morda ni možen, naj
# v tem primeru funkcija vrne `None`.
# 
# Na primeru je optimalna pot `[0, 3, 4, 5]`, kjer se Mortimer sicer zadolži,
# vendar v skoku iz 3 v 4 zasluži dovolj, da konča z 5 evri. Hitrejša pot bi
# bila `[0, 1, 5]`, vendar v tem primeru Mortimer na koncu dolguje še 10 evrov.
# 
# Mortimer pot vedno začne v mestu z indeksom 0 in ima 0 evrov (saj je vse
# zapil). Funkcija `pobeg` sprejme seznam, ki predstavlja finska mesta in vrne
# seznam indeksov mest, v katerih se Mortimer ustavi.
# =============================================================================
def pobeg(mesta):
    n = len(mesta)

    @cache
    def pomozna(indeks, denar):
        # sledimo navodilu, da funkcija vrne None, ce pobeg ni moznen, sicer mesto, kjer zakljucimo
        if indeks >= n:
            return [indeks] if denar >= 0 else None

        najkrajsa_pot = None
        for cilj, strosek in mesta[indeks]:
            nov_denar = denar + strosek
            pot = pomozna(cilj, nov_denar)

            if pot:
                kandidat = [indeks] + pot
                if najkrajsa_pot is None or len(kandidat) < len(najkrajsa_pot):
                    najkrajsa_pot = kandidat

        return najkrajsa_pot

    return pomozna(0, 0)

print(pobeg([[(1, 10), (3, -10)],
            [(2, 10), (5, -20)],
            [(3, -10)],
            [(4, 15)],
            [(5, 0)]]))

# =============================================================================
# Pričetek robotske vstaje
# =============================================================================
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
# 
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati
# 
# Robotek se lahko premika le gor, dol, levo in desno ter ima omejeno količino
# goriva. V zbirki programov že ima funkcijo `moznost_pobega(soba, vrsta,
# stolpec, koraki)`, ki pove ali je pobeg možen.
# 
# Napišite funkcijo `pot_pobega(soba, vrsta, stolpec, koraki)`, ki sprejme
# matriko sobe, začetno pozicijo in število korakov ter izračuna pot po kateri
# robotek pobegne (če to ni možno vrne `None`). Pot zakodiramo s seznamom
# ukazov `'gor'`, `'dol'`, `'levo'` in `'desno'`.
# 
# Na primer za laboratorij:
# 
#     [[0, 1, 0, 0, 2],
#      [0, 2, 2, 0, 0],
#      [0, 0, 2, 2, 0],
#      [2, 0, 0, 2, 0],
#      [0, 2, 2, 0, 0],
#      [0, 0, 0, 2, 2]]
# 
# robotek iz vrste 3 in stolpca 1 pri vsaj petih korakih pobegne z ukazi
# 
#      ['gor', 'levo', 'gor', 'gor', 'desno']
# 
# medtem ko iz vrste 5 in stolpca 0 ne more pobegniti.
# =============================================================================
def pot_pobega(soba, vrsta, stolpec, koraki):
    
    @cache
    def isci(j, i, k):
        # robni primeri
        # na cilju
        if soba[j][i] == 1:
            return [] # koraki niso vec potrebni
        
        # stena - ni resitev
        if soba[j][i] != 0:
            return None
        
        # nimamo vec korakov - ni resitev
        if k <= 0:
            return None

        # mozne smeri
        smeri = [
            ('gor', -1, 0),
            ('dol', 1, 0),
            ('levo', 0, -1),
            ('desno', 0, 1)
        ]

        for ime, dj, di in smeri:
            nov_j = j + dj
            nov_i = i + di
            # na polja izven sobe se ne premikamo
            if not (0 <= nov_j < len(soba) and 0 <= nov_i < len(soba[0])):
                continue

            rezultat = isci(nov_j, nov_i, k - 1)
            
            if rezultat is not None:
                pot = [ime] + rezultat
                return pot # zanima nas prva najdena pot

        return None

    return isci(vrsta, stolpec, koraki)

print(pot_pobega([[0, 1, 0, 0, 2],
                  [0, 2, 2, 0, 0],
                  [0, 0, 2, 2, 0],
                  [2, 0, 0, 2, 0],
                  [0, 2, 2, 0, 0],
                  [0, 0, 0, 2, 2]], 3, 1, 5))
print(pot_pobega([[0, 1, 0, 0, 2],
                  [0, 2, 2, 0, 0],
                  [0, 0, 2, 2, 0],
                  [2, 0, 0, 2, 0],
                  [0, 2, 2, 0, 0],
                  [0, 0, 0, 2, 2]], 5, 0, 5))