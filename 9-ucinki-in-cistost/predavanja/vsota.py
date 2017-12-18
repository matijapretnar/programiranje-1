def ustrezen(n):
    return n % 3 == 0 or n % 5 == 0

for _ in range(100):
    razpon = range(1, 100000)
    veckratniki = [n for n in razpon if ustrezen(n)]
    dolzina = len(veckratniki)
