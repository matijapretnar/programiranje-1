type 'a sez =
    | Prazen
    | Sestavljen of 'a * 'a sez

P(Prazen) /\ (∀ x, xs. P(xs) => P(Sestavljen(x, xs))) => ∀ sez. P(sez)

type 'a drevo =
    | Prazno
    | Sestavljeno of 'a drevo * 'a * 'a drevo

P(Prazno) /\ (∀ l, x, d. P(l) /\ P(d) => P(Sestavljeno(l, x, d))) => ∀ t. P(t)

type boole =
    | Da
    | Ne

P(Da) /\ P(Ne) => ∀ b. P(b)

type 'a mogoce =
    | Nimamo
    | Imamo of 'a

P(Nimamo) /\ ∀ x. P(Imamo x) => ∀ m. P(m)

type 'a veliko_drevo =
    | Prazno
    | Sestavljeno of 'a * (int -> 'a veliko_drevo)

type cuden_tip =
    | VRedu
    | NiVRedu of (cuden_tip -> cuden_tip)

nima indukcije
