##########################################################################
# Hvaležni medved
#
# Pri tej nalogi bomo napisali nekaj funkcij, ki nam bodo v pomoč pri analizi
# literarnih besedil, kot je na primer koroška narodna pripovedka *Hvaležni
# medved*.
#
##########################################################################

odlomek = """Gori nekje v gorah, ne ve se več, ali je bilo pri Macigoju ali
Naravniku, je šivala gospodinja v senci pod drevesom in zibala otroka. Naenkrat
prilomasti - pa prej ni ničesar opazila - medved in ji moli taco, v kateri je
tičal velik, debel trn. Žena se je prestrašila, a medved le milo in pohlevno
godrnja. Zato se žena ojunači in mu izdere trn iz tace. Mrcina kosmata pa zvrne
zibel, jo pobaše in oddide. Čez nekaj časa pa ji zopet prinese zibel, a zvhano
napolnjeno s sladkimi hruškami . Postavil jo je na tla pred začudeno mater in
odracal nazaj v goščavo. "Poglej no", se je razveselila mati, "kakšen hvaležen
medved. Zvrhano zibelko sladkih hrušk mi je prinesel za en sam izdrt trn"."""

##########################################################################
# 1) Sestavite funkcijo najdi_besede(besedilo, podniz), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in vsebujejo niz podniz.
# Zgled:
# 
# >>> najdi_besede(odlomek, 'il')
# {'mil', 'Postavil', 'razveselil', 'opazil', 'bil', 'prestrašil', 'pril'}
##########################################################################


##########################################################################
# 2) Sestavite funkcijo najdi_predpono(besedilo, predpona), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in imajo predpono predpona.
# Zgled:
# 
# >>> najdi_predpono(odlomek, 'zi')
# {'zibala', 'zibel', 'zibelko'}
##########################################################################


##########################################################################
# 3) Sestavite funkcijo najdi_pripono(besedilo, pripona), ki vrne množico
# vseh besed, ki se pojavijo v nizu besedilo in imajo pripono pripona.
# Zgled:
# 
# >>> najdi_pripono(odlomek, 'la')
# {'prestrašila', 'sla', 'zibala', 'razveselila', 'opazila', 'tla', 'šivala'}
##########################################################################


##########################################################################
# 4) Sestavite funkcijo podvojene_crke(besedilo), ki sprejme niz besedilo
# in vrne množico vseh besed, ki vsebujejo podvojene črke. Zgled:
# 
# >>> podvojene_crke('A volunteer is worth twenty pressed men.')
# {'pressed', 'volunteer'}
##########################################################################

