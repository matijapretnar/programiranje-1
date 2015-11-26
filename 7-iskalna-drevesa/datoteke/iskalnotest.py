import unittest
from iskalnodrevo import IskalnoDrevo


class TestIskalnoDrevo(unittest.TestCase):

    def test_poisci_neprazno(self):
        self.assertEqual(IskalnoDrevo().poisci(3), False)

    def test_dodaj_spremeni_prazno(self):
        drevo = IskalnoDrevo()
        self.assertEqual(drevo.prazno, True)
        drevo.dodaj(5)
        self.assertEqual(drevo.prazno, False)

    def test_izbrisi_spremeni_prazno(self):
        drevo = IskalnoDrevo(5)
        self.assertEqual(drevo.prazno, False)
        drevo.izbrisi(5)
        self.assertEqual(drevo.prazno, True)

    def test_najbolj_levi(self):
        drevo = IskalnoDrevo(2, 7, 1, 8, 2, 1, 8)
        self.assertEqual(drevo.najbolj_levi(), 1)

x = IskalnoDrevo(10, 5, 2, 8, 16, 20)
x.dodaj(10)
x.izbrisi(8)
print(x)
unittest.main()
