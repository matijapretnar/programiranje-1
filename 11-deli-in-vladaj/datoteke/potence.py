def pocasni_potenciraj(a, n):
    if n == 0:
        return 1
    else:
        return a * pocasni_potenciraj(a, n - 1)


def potenciraj(a, n):
    if n == 0:
        return 1
    elif n % 2 == 0:
        return potenciraj(a, n // 2) ** 2
    else:
        return a * potenciraj(a, n // 2) ** 2


import timeit

print(timeit.timeit('potenciraj(2, 500)',
      'from __main__ import potenciraj', number=5000))
print(timeit.timeit('pocasni_potenciraj(2, 500)',
      'from __main__ import pocasni_potenciraj', number=5000))
