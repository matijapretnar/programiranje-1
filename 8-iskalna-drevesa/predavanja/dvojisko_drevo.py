class NepraznoDrevo:

    def __init__(self, koren, levo=None, desno=None):
        '''Ustvari neprazno dvojiško drevo z danim korenom in otrokoma.
           - NepraznoDrevo(koren, levo=None, desno=None) ustvari dvojiško drevo z
             danim podatkom v korenu ter levim in desnim otrokom.
             Če kakšen od otrok manjka, se privzame, da je prazen.
        '''
        self.koren = koren
        self.levo = levo
        self.desno = desno

    def __repr__(self):
        niz = 'NepraznoDrevo({}'.format(self.koren)
        if self.levo:
            niz += ', levo={}'.format(self.levo)
        if self.desno:
            niz += ', desno={}'.format(self.desno)
        return niz + ')'

    def velikost(self):
        v = 1
        if self.levo:
            v += self.levo.velikost()
        if self.desno:
            v += self.desno.velikost()
        return v


class Drevo:

    def __init__(self, *args, levo=None, desno=None):
        '''Ustvari dvojiško drevo z danim korenom in otrokoma.
           - Drevo() ustvari prazno dvojiško drevo
           - Drevo(koren, levo=..., desno=...) ustvari dvojiško drevo z
             danim podatkom v korenu ter levim in desnim otrokom.
             Če kakšen od otrok manjka, se privzame, da je prazen.
        '''

        if args:
            assert len(args) == 1
            self.prazno = False
            self.koren = args[0]
            self.levo = levo or Drevo()
            self.desno = desno or Drevo()
        else:
            assert not levo and not desno
            self.prazno = True

    def __repr__(self):
        if self.prazno:
            return 'Drevo()'
        else:
            niz = 'Drevo({}'.format(self.koren)
            if not self.levo.prazno:
                niz += ', levo={}'.format(self.levo)
            if not self.desno.prazno:
                niz += ', desno={}'.format(self.desno)
            return niz + ')'

    def velikost(self):
        if self.prazno:
            return 0
        else:
            return 1 + self.levo.velikost() + self.desno.velikost()

primer_nepraznega_drevesa = NepraznoDrevo(
    4,
    levo=NepraznoDrevo(2),
    desno=NepraznoDrevo(6, levo=NepraznoDrevo(5))
)


primer_drevesa = Drevo(
    4,
    levo=Drevo(2),
    desno=Drevo(6, levo=Drevo(5))
)
