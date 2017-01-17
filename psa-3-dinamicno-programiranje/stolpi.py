def stevilo_stolpov(n):
    return stevilo_modrih(n) + stevilo_rdecih(n)

def stevilo_modrih(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return stevilo_rdecih(n - 1) + stevilo_rdecih(n - 2)

def stevilo_rdecih(n):
    if n < 0:
        return 0
    elif n == 0:
        return 1
    else:
        return stevilo_modrih(n - 2) + stevilo_modrih(n - 3)


print(stevilo_stolpov(41))
