---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: OCaml 4.11
  language: OCaml
  name: ocaml-jupyter
---

# Izračunljivost

## Turingovi stroji

```{prf:definition}
_Turingov stroj_ $M$ je peterica $(\Gamma, \Box, Q, q_0, \delta)$, kjer je:
- $\Gamma$ končna množica simbolov
- $\Box \in \Gamma$ prazni simbol
- $Q$ končna množica stanj
- $q_0 \in Q$ začetno stanje
- $\delta : \Gamma \times Q \rightharpoonup \Gamma \times Q \times \{ \mathtt{L}, \mathtt{R} \}$ prehodna funkcija.
```

V nadaljevanju naj $M$ označuje Turingov stroj podan s peterico $(\Gamma, \Box, Q, q_0, \delta)$. Označimo $f(x) \uparrow$, kadar delna funkcija $f : X \rightharpoonup Y$ na $x \in X$ ni definirana.

```{prf:definition}
_Množica trakov_ $\mathbb{T}_M$ Turingovega stroja $M$ je množica vseh funkcij $t : \mathbb{Z} \to \Gamma$, ki so povsod razen na končno mnogo vrednostih enake praznemu simbolu:

$$
  \mathbb{T} = \{ t : \mathbb{Z} \to \Gamma \mid \exists m \in \mathbb{N}.\ \forall i \in \mathbb{Z}.\ |i| > m \Rightarrow t(i) = \Box\}
$$
```

```{prf:definition}
_Konfiguracija_ Turingovega stroja $M$ je trojica $(q, t, i) \in \mathbb{K}_M = Q \times \mathbb{T} \times \mathbb{Z}$, kjer $q$ opisuje trenutno stanje, $t$ vsebino traku, $i$ pa mesto glave na traku.
```

```{prf:definition}
Delna funkcija $\mathsf{step}_M : \mathbb{K}_M \rightharpoonup \mathbb{K}_M$, ki na konfiguraciji izvede en korak Turingovega stroja.

$$
\mathsf{step}_M(q, t, i) = \begin{cases}
  (q', t[i \mapsto a], i - 1) & \delta(q, t(i)) = (q', a, \mathtt{L}) \\
  (q', t[i \mapsto a], i + 1) & \delta(q, t(i)) = (q', a, \mathtt{R}) \\
  \uparrow & \delta(q, t(i)) \uparrow
\end{cases}
$$

kjer $t[i \mapsto a]$ označuje trak, ki je pri $i$ enak $a$, drugje pa enak kot $t$:

$$
t[i \mapsto a](j) = \begin{cases}
  a & j = i \\
  t(j) & j \ne i
\end{cases}
$$
```

```{prf:definition}
Delna funkcija $\mathsf{run}_M : \mathbb{K}_M \rightharpoonup \mathbb{T}_M$ Turingov stroj izvaja, dokler se ne ustavi:

$$
\mathsf{run}_M(q, t, i) = \begin{cases}
  \mathsf{run}_M(q', t', i') & \mathsf{step}_M(q, t, i) = (q', t', i') \\
  t & \mathsf{step}_M(q, t, i) \uparrow
\end{cases}
$$
```

Pozor: funkcija $\mathsf{run}_M$ je definirana le v primeru, kadar se stroj ustavi, torej če $\mathsf{step}_M$ na neki konfiguraciji ni definiran. Če stroj vedno lahko naredi še en korak, se nikoli ne ustavi in zato $\mathsf{run}_M$ ni definirana.

Fiksirajmo $\Gamma = \{ 0, 1, \Box \}$. Naj bo $\underline{n}$ trak, ki predstavlja število $n \in \mathbb{N}$, zapisano v dvojiškem zapisu. Na primer:

$$\underline{42} = \cdots \Box \Box \underset{\uparrow}{1} 0 1 0 1 0 \Box \Box \cdots$$

Za Turingov stroj $M$ definirajmo funkcijo $M : \mathbb{N} \rightharpoonup \mathbb{N}$, kot
$$
\mathsf{eval}_M(n) = \begin{cases}
  m & \mathsf{run}_M(q_0, \underline{n}, 0) = \underline{m} \\
  \uparrow & \text{sicer}
\end{cases}
$$

```{prf:definition}
Funkcija $f : \mathbb{N} \rightharpoonup \mathbb{N}$ je _izračunljiva_ natanko tedaj, kadar obstaja Turingov stroj $M$, da je $f = \mathsf{eval}_M$.
```

Definicijo lahko razširimo na funkcije več spremenljivk $\mathsf{eval}_M : \mathbb{N}^k \rightharpoonup \mathbb{N}$ tako, da na trak zapišemo več števil, ločenih s presledki

$$
\mathsf{eval}_M(n_1, n_2, \dots, n_k) = \begin{cases}
  m & \mathsf{run}_M(q_0, \underline{n_1}\Box\underline{n_2}\Box\cdots\Box\underline{n_k}, 0) = \underline{m} \\
  \uparrow & \text{sicer}
\end{cases}
$$

## Univerzalni Turingov stroj

```{prf:theorem}
Obstaja univerzalni Turingov stroj $\mathcal{U}$, da za vsak Turingov stroj $M$ obstaja njegova koda $k_M \in \mathbb{N}$, tako da za vse $n \in \mathbb{N}$ velja

$$\mathsf{eval}_\mathcal{U}(k_M, n) \simeq \mathsf{eval}_M(n)$$
```

V resnici lahko univerzalni Turingov stroj simulira $M$ na vseh vhodih, ne le na $\underline{n}$, ampak zgornja trditev je enostavnejša in hkrati dovolj za našo uporabo.

## Ustavitveni problem

```{prf:theorem}
Ne obstaja Turingov stroj $\mathcal{H}$, da bi za vsak Turingov stroj $M$ veljalo

$$
\mathsf{eval}_\mathcal{H}(k_M, n) = \begin{cases}
  0 & \mathsf{eval}_M(n) \uparrow \\
  1 & \mathsf{eval}_M(n) \downarrow
\end{cases}
$$
```

```{prf:proof}
Recimo, da tak stroj obstaja. Tedaj lahko definiramo tudi stroj $X$, ki se bo na vhodu $k_M$ obnašal ravno nasprotno kot $M$: če se bo $M$ na $k_M$ ustavil, bo $X$ divergiral, in obratno.

$$
\mathsf{eval}_X(k_M) = \begin{cases}
    \uparrow & \mathsf{eval}_\mathcal{H}(k_M, k_M) = 1 \\
    0 & \mathsf{eval}_\mathcal{k_M, k_M} = 0
\end{cases}
$$

Toda tudi stroj $X$ mora imeti neko kodo $k_X$, zato velja

$$\begin{align*}
\mathsf{eval}_X(k_X) &= 
\begin{cases}
    \uparrow & \mathsf{eval}_\mathcal{H}(k_X, k_X) = 1 \\
    0 & \mathsf{eval}_\mathcal{k_X, k_X} = 0
\end{cases} \\
&= \begin{cases}
    \uparrow & \mathsf{eval}_X(k_X) \downarrow \\
    0 & \mathsf{eval}_X(k_X) \uparrow
\end{cases}
\end{align*}$$

Torej $X$ na $k_X$ divergira natanko takrat, kadar konvergira in obratno. To pa je protislovje.

```