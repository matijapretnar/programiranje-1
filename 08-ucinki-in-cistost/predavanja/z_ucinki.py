sez = [1, 2, 3]

def f(x):
    sez = sez.copy()
    sez.append(x)
    return len(sez)

def g(x):
    return f(x) + f(x)
