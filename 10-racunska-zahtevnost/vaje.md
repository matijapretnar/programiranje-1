Ponovimo definicijo velikega O:
$$
f \in O(g) \iff \exists x_0 \exists M. \forall x > x_0. f(x) \leq M * g(x)
$$

1. $h \in O(f) \implies a * h \in O(f)$

2. $h \in O(f) \implies h \in O(f + g)$ za vsak $g$.

3. $f_1 \in O(g_1) \land f_2 \in O(g_2) \implies f_1 + f_2 \in O(g_1 + g_2)$

4. $h \in O(f_1 + f_2) \implies h \in O(max(f_1, f_2))$

5. $O(log_b(n)) = O(log_2(n))$

6. $O(\log(n!)) = O(n * \log(n))$ (Za $n > 10$)
