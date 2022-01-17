from functools import cache

LOCILO = " ~> "


def stakni(pot1, pot2):
    return pot1 + LOCILO + pot2


def dolzina_poti(pot):
    return pot.count(LOCILO)


def krajsa_od(pot1, pot2):
    return pot1 if dolzina_poti(pot1) <= dolzina_poti(pot2) else pot2


def pot_od_prazne_besede(beseda):
    return "" if beseda == "" else stakni(pot_od_prazne_besede(beseda[1:]), beseda)


def pot_do_prazne_besede(beseda):
    return "" if beseda == "" else stakni(beseda, pot_do_prazne_besede(beseda[1:]))


def pot_dolzine_ena(beseda):
    return beseda


def dodaj_znak_vsakemu_koraku(znak, pot):
    return znak + pot.replace(LOCILO, LOCILO + znak)

def cache(f):
    rezultati = {}
    def mem_f(*args):
        if args not in rezultati:
            rezultati[args] = f(*args)
        return rezultati[args]
    return mem_f

@cache
def najkrajsa_pot(beseda1, beseda2):
    if beseda1 == "":
        return pot_od_prazne_besede(beseda2)
    elif beseda2 == "":
        return pot_do_prazne_besede(beseda1)
    elif beseda1[0] == beseda2[0]:
        # Xabc ~> ... ~> Xxyz
        # kjer je abc ~> ... ~> xyz
        pot_med_repoma = najkrajsa_pot(beseda1[1:], beseda2[1:])
        return dodaj_znak_vsakemu_koraku(beseda1[0], pot_med_repoma)
    else:
        # Xabc ~> ... ~> Ydef

        # X zamenjamo z Y
        # Xabc ~> Yabc ~> Y... ~> Ydef
        pot_med_repoma = najkrajsa_pot(beseda1[1:], beseda2[1:])
        zamenjamo = stakni(
            pot_dolzine_ena(beseda1),
            dodaj_znak_vsakemu_koraku(beseda2[0], pot_med_repoma),
        )
        # X pobrišemo
        # Xabc ~> abc ~> ... ~> Ydef
        pobrisemo = stakni(
            pot_dolzine_ena(beseda1), najkrajsa_pot(beseda1[1:], beseda2)
        )
        # Y dodamo
        # Xabc ~> ... ~> def ~> Ydef
        dodamo = stakni(najkrajsa_pot(beseda1, beseda2[1:]), pot_dolzine_ena(beseda2))
        return krajsa_od(zamenjamo, krajsa_od(pobrisemo, dodamo))

najkrajsa_pot = cache(najkrajsa_pot)

print(najkrajsa_pot("KOLO", "KLON"))
print(najkrajsa_pot("kolesar", "vojna klonov"))
print(najkrajsa_pot("kolesarji radi kolesarijo", "kloni se radi borijo v vojni klonov"))
