def f(sez):
    sez.append(1)
    return len(sez)

def g(sez):
    return f(sez) + f(sez)
