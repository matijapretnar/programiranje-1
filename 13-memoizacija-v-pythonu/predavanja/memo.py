kvadrati = {}
def mem_kvadrat(x):
    if x not in kvadrati:
        print(f'Računam {x}^2...')
        y = x ** 2
        kvadrati[x] = y
    return kvadrati[x]

fibonaccijeva = {}
def mem_fibonacci(x):
    if x not in fibonaccijeva:
        print(f'Računam fib({x})...')
        match x:
            case 0:
                y = 0
            case 1:
                y = 1
            case _:
                y = mem_fibonacci(x - 1) + mem_fibonacci(x - 2)
        fibonaccijeva[x] = y
    return fibonaccijeva[x]
