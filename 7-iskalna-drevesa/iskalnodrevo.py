class Drevo:
    def __str__(self, zamik=""):
        if self.prazno:
            return zamik + "x"
        else:
            return "\n".join([
                self.desno.__str__(zamik=zamik+"   "),
                zamik + str(self.vrednost),
                self.levo.__str__(zamik=zamik+"   ")
            ])


class IskalnoDrevo(Drevo):
    def __init__(self, *elementi):
        self.prazno = True
        for x in elementi:
            self.dodaj(x)

    def dodaj(self, x):
        if self.prazno:
            self.prazno = False
            self.levo = IskalnoDrevo()
            self.vrednost = x
            self.desno = IskalnoDrevo()
        elif x < self.vrednost:
            self.levo.dodaj(x)
        elif x > self.vrednost:
            self.desno.dodaj(x)
        else:
            pass

    def poisci(self, x):
        if self.prazno:
            return False
        elif x < self.vrednost:
            return self.levo.poisci(x)
        elif x > self.vrednost:
            return self.desno.poisci(x)
        else:
            return True

    def izbrisi(self, x):
        if self.prazno:
            pass
        elif x < self.vrednost:
            self.levo.izbrisi(x)
        elif x > self.vrednost:
            self.desno.izbrisi(x)
        else:
            self.izbrisiKoren()

    def najbolj_levi(self):
        if self.levo.prazno:
            return self.vrednost
        else:
            return self.levo.najbolj_levi()

    def izbrisiKoren(self):
        if self.levo.prazno:
            self.zamenjaj(self.desno)
        elif self.desno.prazno:
            self.zamenjaj(self.levo)
        else:
            vrednost0 = self.desno.najbolj_levi()
            self.vrednost = vrednost0
            self.desno.izbrisi(vrednost0)

    def zamenjaj(self, drevo):
        if drevo.prazno:
            if self.prazno:
                pass
            else:
                del self.vrednost
                del self.levo
                del self.desno
                self.prazno = True
        else:
            self.vrednost = drevo.vrednost
            self.levo = drevo.levo
            self.desno = drevo.desno
            self.prazno = drevo.prazno
