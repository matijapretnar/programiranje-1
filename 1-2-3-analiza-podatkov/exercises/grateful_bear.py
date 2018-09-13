###############################################################################
# Grateful Bear
#
# In this exercise we will write a few functions that help us analyse literary
# texts, such as the Carinthian folk tale *Grateful Bear*.
#
###############################################################################

test_text = """Gori nekje v gorah, ne ve se več, ali je bilo pri Macigoju ali
Naravniku, je šivala gospodinja v senci pod drevesom in zibala otroka. Naenkrat
prilomasti - pa prej ni ničesar opazila - medved in ji moli taco, v kateri je
tičal velik, debel trn. Žena se je prestrašila, a medved le milo in pohlevno
godrnja. Zato se žena ojunači in mu izdere trn iz tace. Mrcina kosmata pa zvrne
zibel, jo pobaše in oddide. Čez nekaj časa pa ji zopet prinese zibel, a zvhano
napolnjeno s sladkimi hruškami . Postavil jo je na tla pred začudeno mater in
odracal nazaj v goščavo. "Poglej no", se je razveselila mati, "kakšen hvaležen
medved. Zvrhano zibelko sladkih hrušk mi je prinesel za en sam izdrt trn"."""

###############################################################################
# 1) Write a function [find_words] that returns a set of all the  words in a
#    string containing the given substring.
#
# Hint: Use the regex character for the boundary [\b].
#
# >>> find_words(test_text, 'de')
# {'izdere', 'debel', 'oddide', 'začudeno'}
###############################################################################


###############################################################################
# 2) Write a function [find_prefix] which returns the set of all words in a
#    string starting with the given prefix.
#
# >>> find_prefix(test_text, 'zi')
# {'zibala', 'zibel', 'zibelko'}
###############################################################################


###############################################################################
# 3) Write a function [find_suffix] which returns the set of all words in a
#    string ending with the given suffix.
#
# >>> find_suffix(test_text, 'la')
# {'zibala', 'razveselila', 'prestrašila', 'šivala', 'opazila', 'tla'}
###############################################################################


###############################################################################
# 4) Write a function [double_letters] that returns the set of words in a
#    string that contain the same letter twice consecutively.
#
# >>> double_letters('A volunteer is worth twenty pressed men.')
# {'volunteer', 'pressed'}
###############################################################################
