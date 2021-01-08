from functools import lru_cache

@lru_cache(maxsize=None)
def levenshteinova_razdalja(beseda1, beseda2):
    # obe prazni
    if len(beseda1) == 0 and len(beseda2) == 0:
        return 0
    # prva prazna
    elif len(beseda1) == 0:
        # dodamo ostale črke druge
        return len(beseda2)
    # druga prazna
    elif len(beseda2) == 0:
        # dodamo ostale črke prve
        return len(beseda1)
    # prvi črki enaki
    # Xxxx ~> ... ~> Xyyy kjer gre xxx ~> ... ~> yyy
    elif beseda1[0] == beseda2[0]:
        return levenshteinova_razdalja(beseda1[1:], beseda2[1:])
    else:
        # Xxxx ~> Yxxx ~> ... ~> Yyyy kjer gre xxx ~> ... ~> yyy
        zamenjamo = levenshteinova_razdalja(beseda1[1:], beseda2[1:])
        # Xxxx ~> xxx ~> ... ~> Yyyy
        pobrisemo = levenshteinova_razdalja(beseda1[1:], beseda2)
        # Xxxx ~> YXxxx ~> ... ~> Yyyy
        dodamo = levenshteinova_razdalja(beseda1, beseda2[1:])
        return 1 + min(zamenjamo, pobrisemo, dodamo)

@lru_cache(maxsize=None)
def levenshteinova_pot(beseda1, beseda2):
    """Vrne najkrajšo pot med besedama"""
    # obe prazni
    if len(beseda1) == 0 and len(beseda2) == 0:
        return [""]
    # prva prazna
    elif len(beseda1) == 0:
        # dodamo ostale črke druge
        return [beseda2[:i] for i in range(len(beseda2) + 1)]
    # druga prazna
    elif len(beseda2) == 0:
        # dodamo ostale črke prve
        return [beseda1[:i] for i in range(len(beseda1) + 1)]
    # prvi črki enaki
    # Xxxx ~> ... ~> Xyyy kjer gre xxx ~> ... ~> yyy
    elif beseda1[0] == beseda2[0]:
        xxx_yyy = levenshteinova_pot(beseda1[1:], beseda2[1:])
        return [beseda1[0] + korak for korak in xxx_yyy]
    else:
        # Xxxx ~> Yxxx ~> ... ~> Yyyy
        xxx_yyy = levenshteinova_pot(beseda1[1:], beseda2[1:])
        Yxxx_Yyyy = [beseda2[0] + korak for korak in xxx_yyy]
        # Xxxx ~> xxx ~> ... ~> Yyyy
        xxx_Yyyy = levenshteinova_pot(beseda1[1:], beseda2)
        # Xxxx ~> YXxxx ~> ... ~> Yyyy
        Xxxx_yyy = levenshteinova_pot(beseda1, beseda2[1:])
        YXxxx_Yyyy = [beseda2[0] + korak for korak in xxx_yyy]
        return [beseda1] + min(Yxxx_Yyyy, xxx_Yyyy, YXxxx_Yyyy, key=len)

print(levenshteinova_razdalja("Programiranje 1", "Uvod v programiranje"))
print(levenshteinova_pot("Programiranje 1", "Uvod v programiranje"))
print(levenshteinova_pot("matematika", "fizika"))