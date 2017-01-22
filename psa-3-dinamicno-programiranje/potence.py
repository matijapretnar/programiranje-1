def pocasni_potenciraj_rek(a, n):
    if n == 0:
        return 1
    else:
        return a * pocasni_potenciraj_rek(a, n - 1)


def pocasni_potenciraj_imp(a, n):
    potenca = 1
    for _ in range(n):
        potenca *= a
    return potenca


def potenciraj(a, n):
    if n == 0:
        return 1
    elif n % 2 == 0:
        return potenciraj(a ** 2, n // 2)
    else:
        return a * potenciraj(a ** 2, n // 2)


def pocasni_fibonacci(n):
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a


def zmnozi(a, b):
    return [[a[0][0] * b[0][0] + a[0][1] * b[1][0],
             a[0][0] * b[0][1] + a[0][1] * b[1][1]],
            [a[1][0] * b[0][0] + a[1][1] * b[1][0],
             a[1][0] * b[0][1] + a[1][1] * b[1][1]]]


def potenciraj_matriko(a, n):
    if n == 0:
        return [[1, 0], [0, 1]]
    elif n % 2 == 0:
        return potenciraj_matriko(zmnozi(a, a), n // 2)
    else:
        return zmnozi(a, potenciraj_matriko(zmnozi(a, a), n // 2))


def fibonacci(n):
    return potenciraj_matriko([[1, 1], [1, 0]], n - 1)[0][0]
