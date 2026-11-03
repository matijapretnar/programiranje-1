# Asimptotska notacija

Ponovimo definicije s predavanj:

$$f \in O(g) \iff \exists C > 0, n_0.\ \forall n \ge n_0.\ f(n) \le C \cdot g(n)$$

$$f \in \Omega(g) \iff \exists c > 0, n_0.\ \forall n \ge n_0.\ f(n) \ge c \cdot g(n)$$

$$f \in \Theta(g) \iff f \in O(g) \land f \in \Omega(g)$$

- Vse funkcije so nenegativne.
- Brez dokaza lahko uporabite, da sta relaciji $O$ in $\Theta$ refleksivni in tranzitivni (npr. velja $f \in O(f)$, iz $f \in O(g)$ in $g \in O(h)$ pa sledi $f \in O(h)$).

## Pravila za računanje

Dokažite naslednje trditve. Pri vsaki premislite še, ali velja tudi, če $O$ povsod zamenjamo s $\Theta$.

1. $h \in O(f) \implies a \cdot h \in O(f)$ za vsako konstanto $a > 0$.

2. $h \in O(f) \implies h \in O(f + g)$ za vsako funkcijo $g$.

3. $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 + f_2 \in O(g_1 + g_2)$

4. $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 \cdot f_2 \in O(g_1 \cdot g_2)$

## Ocene

5. $f_1 + f_2 \in \Theta(\max(f_1, f_2))$, kjer je $\max(f_1, f_2)$ funkcija $n \mapsto \max(f_1(n), f_2(n))$.

6. S pomočjo pravil iz prejšnjih nalog, pokažite, da je $\frac{n (n + 1)}{2} \in \Theta(n^2)$. 

7. $\log_b n \in \Theta(\log_2 n)$ za vsako osnovo $b > 1$.

8. $\log_2 (n!) \in \Theta(n \log_2 n)$

    Namig: Za zgornjo mejo primerjajte $n!$ z $n^n$. Za spodnjo mejo opazite, da je vsaj polovica faktorjev v $n! = 1 \cdot 2 \cdots n$ večjih ali enakih $n / 2$.

9. (Dodatna naloga) $2^{n + 1} \in \Theta(2^n)$, vendar $2^{2n} \notin O(2^n)$.
