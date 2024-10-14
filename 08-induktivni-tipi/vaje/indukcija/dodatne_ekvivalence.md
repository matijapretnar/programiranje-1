1. `@` je associativen
	Dokazujemo za vsak `l1, l2, l3` velja `(l1 @ l2) @ l3 == l1 @ (l2 @ l3)`.
	Indukcija na `l1`, $\forall. \ l2, l3$
```
1. l1 = []
  ([] @ l2) @ l3  
    = ( [] identiteta ) =
  l2 @ l3
    = ( [] identiteta )
  [] @ (l2 @ l3)
2. l1 = x::xs
  ((x::xs) @ l2) @ l3
    = (def @ -> ) =
  (x::(xs @ l2)) @ l3
    = (def @ -> ) =
  x :: ((xs @ l2) @ l3)
    = (IH) =
  x :: (xs @ (l2 @ l3))
    = (def @ <-)
  (x:: xs) @ (l2 @ l3)
    = (def l1 <-)
  l1 @ (l2 @ l3)
```
2. Elementi v drevesu
```ocaml
let rec elementi = function
  | Prazno -> []
  | Sestavljeno (l, x, d) -> elementi l @ [x] @ elementi d

let elt' t =
  let rec aux acc = function 
    | Prazno -> []
    | Sestavljeno (l, x, r) -> 
	   aux (x::(aux acc r)) l
  in
  aux [] t
```
Za vsak seznam $l$ velja: `elt' t == elementi t`
Podobno kot na predavanjih trditev ne moremo dokazati direktno, ampak si pomagamo z indukcijo za funkcijo `aux`. Še več, nekoliko posplošimo trditev, kar nam pride prav v induktivnem koraku.
Dokazujemo torej: "za vsak `t` velja za vsak `acc`:
`aux acc t == (elementi t) @ acc`". Specifično, za `acc = []` bo potem veljalo `elt' t == aux [] t == elementi t`, kar je ravno zahteva naloge. Pri dokazovanju smo nekoliko bolj pazljivi in za nek `t` dokazujemo da velja za vsak `acc`...

Imamo nek `t` in dokazujemo z indukcijo na obliko `t`, trditev: za vsak  `acc` velja `aux acc t == (elementi t) @ acc`.
```
1. t = Prazno:
  aux acc Prazno 
    = (def aux->) = 
  acc 
    = ([] je enota za @ z leve) =
  [] @ acc
    = (def elementi <-) =
  (elementi Prazno) @ acc
2. t = Sestavljeno(l, x, r)
  aux acc (Sestavljeno(l,x,r)) 
    = (def aux->) =
  aux (x::(aux acc r)) l
    = (IH za: aux acc r) =
  aux (x :: ((elementi r) @ acc)) l
    = (def [_] @ _) =
  aux ([x] @ (elementi r) @ acc) l
    = (IH za: aux ([x] @ (elementi r) @ acc) l)..tukaj rabimo generaliziran forall =
  (elementi l) @ (([x] @ (elementi r) @ acc))
    = (associativnosti @)
  ((elementi l) @ [x] @ (elementi r)) @ acc
    = (def elementi (Sestavljeno(l, x, r)) <-) =
  (elementi Sestavljeno(l, x, r)) @ acc
    = (def t <-) =
  (elementi t) @ acc
```
