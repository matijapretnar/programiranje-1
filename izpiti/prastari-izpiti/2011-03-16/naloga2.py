class Seznam():
    def __init__(self):
        self.prazen = True

    def __repr__(self):
        return ("()" if self.prazen else "({0},{1})".format(self.glava, self.rep))

    def dodaj(self, x):
        if self.prazen:
            self.prazen = False
            self.glava = x
            self.rep = Seznam()
        else:
            self.rep.dodaj(x)
        return self

    def dolzina(self):
        return (0 if self.prazen else 1 + self.rep.dolzina())
