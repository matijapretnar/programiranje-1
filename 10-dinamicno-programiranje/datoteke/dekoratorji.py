depth = 0


def debug(f):

    def debug_f(*args):
        global depth
        print("{}{}({})".format(depth * " ", f.__name__, ", ".join(repr(arg) for arg in args)))
        depth += 2
        res = f(*args)
        print("{}= {}".format(depth * " ", res))
        depth -= 2
        return res

    return debug_f


def memo(f):
    izracunane = {}

    def memo_f(*args):
        if args not in izracunane:
            izracunane[args] = f(*args)
        return izracunane[args]

    return memo_f


def fib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)

print(fib(10))