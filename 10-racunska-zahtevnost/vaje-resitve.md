Ponovimo definicijo velikega O:

$$
f \in O(g) \iff \exists M. \exists x_0 \forall x > x_0. f(x) \leq M * g(x)
$$

1. $ h \in O(f) \implies a * h \in O(f)$
    
    $x_0' = x_0$ in $M' = a * M$. Zahtevo iz definicije očitno izpolnjuje.

2. $h \in O(f) \implies h \in O(f + g)$ za vsak $g$.
    
    Ker $g \leq 0$ velja $f \leq f + g$. Torej $f \in O(f + g)$.

3. $f \in O(g_1) \land f \in O(g_2) \implies f_1 + f_2 \in O(g_1 + g_2)$


    Iz definicije vzemimo $x_0^1, M_1$ in $x_0^2, M_2$ za $f_1, g_1$ in $f_2, g_2$.

    Naj bo $x_0 = max(x_0^1, x_0^2)$. Računamo:

    $$
    f_1(x) + f_2(x) < M_1 * g_1(x) + M_2 * g_2(x) < (M_1 + M_2) * (g_1(x) + g_2(x))
    $$

4. $h \in O(f_1 + f_2) \implies h \in O(max(f_1, f_2))$

    Iz definicije vzemimo $x_0, M$ za $f_1 + f_2$.

    Potrebujemo (od nekje naprej):

    $$
    f_1(x) + f_2(x) \leq M' * max(f_1(x), f_2(x))
    $$

    Trik je v tem, da lahko $M'$ pomonožimo s poljubno konstanto.
    
    Očitno $3 * max(f_1, f_2) > f_1 + f_2$, torej $M' = 3 * M$ in $x_0$ ostane enak.

    Preverimo še formalno: Naj bo $x > x_0$.

    $h(x) < M * (f_1(x) + f_2(x)) < 3 * M * max(f_1(x), f_2(x))$

5. $O(log_b(n)) = O(log_2(n))$

    $log_b(n) = \frac{log_2(n)}{log_2(b)} = \frac{1}{log_2(b)} * log_2(n)$, kjer je ulomek konstanta.

    Potrebni konstanti **v obe smeri** sta enostavni.


6. $O(n!) = O(n * \log(n))$ (Za $n > 10$)

    Očitno lahko vzamemo logaritem z osnovo $2$ (glej prejšnjo točko).

    $n! = n * (n-1) * \ldots * 1 < n * n * \ldots * n = n^n$
    in 
    $\log (n!) < n \log(n)$.

    Podobno v drugo smer:
    $n! = n * (n-1) * \ldots * 1 > n/2 * n/2 * \ldots * n/2 = (n/2)^n$
    in $\log(n!) > n/2 \log(n/2) = \frac{1}{2} n (\log(n) - 1)$.



