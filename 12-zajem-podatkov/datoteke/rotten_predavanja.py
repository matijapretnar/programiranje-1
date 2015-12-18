from utils import *
import re

shrani('http://www.rottentomatoes.com/top/bestofrt/top_100_action__adventure_movies/', 'podatki/akcije.html')

vzorec_filma = re.compile(
    r'<tr> <td class="bold">(?P<mesto>\d+)\..*?(?P<ocena>\d+)%.*?">(?P<naslov>.*?) \((?P<leto>\d{4})'
)

for ujemanje in re.finditer(vzorec_filma, vsebina_datoteke('podatki/akcije.html')):
    print(ujemanje.groupdict())
