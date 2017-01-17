ze_izracunani = {}

def najdaljsi_podpalindrom(niz):
    if niz in ze_izracunani:
        return ze_izracunani[niz]
    else:
        if len(niz) <= 1:
            odgovor = niz
        elif niz[0] == niz[-1]:
            odgovor = niz[0] + najdaljsi_podpalindrom(niz[1:-1]) + niz[-1]
        else:
            levi = najdaljsi_podpalindrom(niz[:-1])
            desni = najdaljsi_podpalindrom(niz[1:])
            if len(levi) < len(desni):
                odgovor = desni
            else:
                odgovor = levi
        ze_izracunani[niz] = odgovor
        return odgovor

print(najdaljsi_podpalindrom('otorinolaringologotorinolaringol'
    'ogotorinolaringologotorinolaringologotorinolaringo'
    'logotorinolaringologotorinolaringologotorinolarin'
    'gologotorinolaringologotorinolaringologotorinolaring'
    'ologotorinolaringologotorinolaringologotorinolaringo'
    'ologotorinolaringologotorinolaringologotorinolaringo'
    'ologotorinolaringologotorinolaringologotorinolaringo'
    'ologotorinolaringologotorinolaringologotorinolaringo'
    'logotorinolaringolog'))
