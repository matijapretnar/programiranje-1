def povprecja(sez, k):
  return [sum(sez[i:i + k]) / k for i in range(len(sez) - k + 1)]

print(povprecja([1, 2, 3, 4], 2))
print(povprecja([2, 3, 4, 5, 6], 3))

def iz_povprecij(sez, povprecja):
    k = len(sez) + 1
    skoraj_povprecje = sum(sez)
    for i, povprecje in enumerate(povprecja):
        naslednji = k * povprecje - skoraj_povprecje
        skoraj_povprecje += naslednji - sez[i]
        sez.append(skoraj_povprecje)
    return sez


Sestavite funkcijo \texttt{naloga3a[sez_, {i_, j_}]}, ki sprejme seznam \texttt{sez} in par indeksov \texttt{i}, \texttt{j}.
Funkcija naj vrne seznam, ki je enak seznamu \texttt{sez}, le da sta v njem elementa na mestih \texttt{i} in \texttt{j} zamenjana.

Sestavite funkcijo \texttt{naloga3b[trans_, n_]}, ki sprejme seznam transpozicij