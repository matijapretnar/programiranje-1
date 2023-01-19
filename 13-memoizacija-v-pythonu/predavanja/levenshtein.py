from functools import cache

def pot_od_prazne_besede(beseda):
    return [""] if beseda == "" else pot_od_prazne_besede(beseda[1:]) + [beseda]


def pot_do_prazne_besede(beseda):
    return [""] if beseda == "" else [beseda] + pot_do_prazne_besede(beseda[1:])


def dodaj_znak_vsakemu_koraku(znak, pot):
    return [znak + korak for korak in pot]

@cache
def najkrajsa_pot(beseda1, beseda2):
    print(beseda1, beseda2)
    if beseda1 == "":
        return pot_od_prazne_besede(beseda2)
    elif beseda2 == "":
        return pot_do_prazne_besede(beseda1)
    elif beseda1[0] == beseda2[0]:
        # Xabc ~> ... ~> Xdef
        # kjer je abc ~> ... ~> def
        pot_med_repoma = najkrajsa_pot(beseda1[1:], beseda2[1:])
        return dodaj_znak_vsakemu_koraku(beseda1[0], pot_med_repoma)
    else:
        # Xabc ~> ... ~> Ydef

        # X zamenjamo z Y
        # Xabc ~> Yabc ~> Y... ~> Ydef
        zamenjamo = najkrajsa_pot(beseda2[0] + beseda1[1:], beseda2)

        # X pobrišemo
        # Xabc ~> abc ~> ... ~> Ydef
        pobrisemo = najkrajsa_pot(beseda1[1:], beseda2)

        # Y dodamo na začetek
        # Xabc ~> YXabc ... ~> Ydef
        dodamo = najkrajsa_pot(beseda2[0] + beseda1, beseda2)

        return [beseda1] + min(pobrisemo, dodamo, zamenjamo, key=len)
