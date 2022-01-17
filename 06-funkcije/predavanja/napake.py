def f1(x : int) -> int:
    return x + 10

def f2(x):
    return f1(x)

def f3(x):
    return f2(x)

def f4(x : int) -> int:
    return f3(x)

def g(x):
    return f4("True") + x

