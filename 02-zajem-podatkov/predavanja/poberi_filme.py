# https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start=1&ref_=adv_nxt
# https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start=251&ref_=adv_nxt
# https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start=501&ref_=adv_nxt
# https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start=751&ref_=adv_nxt


# import requests
# for i in range(4):
#     url = f"https://www.imdb.com/search/title/?title_type=feature&sort=num_votes,desc&count=250&start={250 * i + 1}&ref_=adv_nxt"
#     odziv = requests.get(url)
#     if odziv.status_code == 200:
#         print(url)
#         with open(f"stran-{i}.html", "w") as f:
#             f.write(odziv.text)
#     else:
#         print("Prišlo je do napake")

from preberi_podatke import vzorec_bloka, izloci_podatke_filma
import json

filmi = []
count = 0
for i in range(4):
    with open(f"stran-{i}.html") as f:
        vsebina = f.read()
    for blok in vzorec_bloka.finditer(vsebina):
        film = izloci_podatke_filma(blok.group(0))
        count += 1
        filmi.append(film)
print(count)
with open("filmi.json", "w") as f:
    json.dump(filmi, f, ensure_ascii=False, indent=4)

import csv

with open("filmi.csv", "w") as f:
    pisatelj = csv.writer(f)
    pisatelj.writerow(["id", "naslov", "leto", "ocena"])
    for film in filmi:
        id, naslov, leto, ocena = film['id'], film['naslov'], film['leto'], film['ocena']
        pisatelj.writerow([id, naslov, leto, ocena])
