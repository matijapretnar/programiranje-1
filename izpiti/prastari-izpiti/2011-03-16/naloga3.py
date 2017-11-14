def f(i,j):
    if j == 0: return 1
    elif i == 0: return 0
    else:
        return f(i-1,j) + i * f(i,j-1)

def g(i,j):
    m = {}

    def h(i,j):
        if j == 0: return 1
        elif i == 0: return 0
        else:
            if (i,j) in m: return m[(i,j)]
            else:
                m[(i,j)] = h(i-1,j) + i * h(i,j-1)
                return m[(i,j)]

    return h(i,j)
