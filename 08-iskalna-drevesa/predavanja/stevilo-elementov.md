# Najmanjše število elementov v AVL drevesu

N(h) ... najmanjše število elementov v AVL drevesu višine h

N(0) = 0
N(1) = 1
N(h) >= 1 + N(h - 1) + N(h - 2)


N(h) >= F(h)
. . . . . . 

0 DA
1 DA
N(h)
    >= 1 + N(h - 1) + N(h - 2)
    > N(h - 1) + N(h - 2)
    >= F(h - 1) + F(h - 2)
    = F(h)