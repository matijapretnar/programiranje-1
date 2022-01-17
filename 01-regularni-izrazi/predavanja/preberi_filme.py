import re

with open("250-najbolj-znanih-filmov.html") as dat:
    vsebina = dat.read()

vzorec = re.compile(
    r'<a href="/title/tt'
    r'(?P<id>\d+)'
    r'/\?ref_=adv_li_tt">(?P<naslov>.+?)</a>\s*'
    r'<span class="lister-item-year text-muted unbold">'
    r'(\([IVXLCDM]+\) )?'  # če je več filmov v istem letu, dobijo rimske številke
    r'\((?P<leto>\d+)\)'  # vzorec za leto
    r'</span>'
)

for i, ujemanje in enumerate(vzorec.finditer(vsebina), 1):
    print(i, ujemanje.groupdict())
