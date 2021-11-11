def fakulteta(n):
    if n == 0:
        return 1
    else:
        return n * fakulteta(n - 1)

def fakulteta_repna(n, acc=1):
    if n == 0:
        return acc
    else:
        return fakulteta_repna(n - 1, n * acc)

# f5
# f2
# f1