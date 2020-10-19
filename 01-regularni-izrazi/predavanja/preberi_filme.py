import re

with open('250-najbolj-znanih-filmov.html') as f:
    vsebina = f.read()

vzorec = (
    r'<a href="/title/tt'
    r'(?P<id>\d{7})'  # ID ima sedem števk
    r'/\?ref_=adv_li_tt"\n>'  # neka šara vmes med id-jem in naslovom
    r'(?P<naslov>.*?)'  # zajamemo naslov
    r'</a>'
    r'\s+'
    r'<span class="lister-item-year text-muted unbold">'
    r'(\([IVXLCDM]+\) )?'
    r'\((?P<leto>.*?)\)'
)

count = 0
for zadetek in re.finditer(vzorec, vsebina):
    print(zadetek.groupdict())
    count += 1
print(count)

