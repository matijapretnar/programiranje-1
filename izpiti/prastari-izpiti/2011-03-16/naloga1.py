def najpogostejsi(a):
    d = {}
    m = None
    for k in a:
        d[k] = 1 + d.get(k,0)
        if m is None or d[k] > d[m]:
            m = k
    return m

def najpogostejsi_dodatna(a):
    d = {}
    m = None
    for (j,k) in enumerate(a):
        if k in d:
            d[k].append(j)
        else: 
            d[k] = [j]
        if m is None or len(d[k]) > len(d[m]):
            m = k
    return d[m]

