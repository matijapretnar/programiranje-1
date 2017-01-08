"""
Implementacije algoritmov za urejanje
"""

def naivno_uredi(l):
    for i in range(0, len(l) - 1):
        menjava = False
        for j in range(0, len(l) - i - 1):
            if l[j] > l[j + 1]:
                l[j], l[j + 1] = l[j + 1], l[j]  # swap
            menjava = True
        if menjava == False:
            break
    return l

def vgrajeni_sort(l):
    l.sort()

def hitro_uredi_z_novimi_seznami(l):
    pass  # TODO

def hitro_uredi_na_mestu(l):
    pass  # TODO
        
def uredi_z_zlivanjem(l):
    pass  # TODO
