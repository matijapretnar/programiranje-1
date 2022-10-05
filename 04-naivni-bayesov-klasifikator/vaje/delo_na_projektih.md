# Nekaj najpogostejših težav pri prenosu podatkov in možne rešitve

1. *Ko poskušam z requesti prenesti spletno stran mi javi čudno napako, ali pa je html popolnoma drugačen kot pri ročnem pogledu.*
To je običajno znak, da vas poskuša spletna stran odvrniti od tega, da avtomatsko prenašate podatke.
Nekatere strani se poskušajo na različne načine zaščititi pred pajki, ki prenašajo podatke. Za to, da prikrijemo dejstvo, da smo računalnik, je več načinov. Katere je potrebno uporabiti je predvsem odvisno od tega, kaj vse spletna stran gleda. Primer take strani je recimo [avto.net](avto.net)
   * Nastavite glavo `User-Agent`. V requestih to najlažje naredimo kar z dodatnim argumentom

   ```python
   headers = {"User-Agent": "Izbrani User-Agent"}
   response = requests.get(url, headers=headers)
   ```

   Pri tem je `User-Agent` smiselno nastaviti na vrednost kot jo imate v brskalniku, ali pa se predstavite kot kdo [drug](https://deviceatlas.com/blog/list-of-user-agent-strings#desktop).
   * Če tudi to ni dovolj in je potrebno uporabiti [sejo](https://requests.readthedocs.io/en/master/user/advanced/#session-objects), ki shranjuje tudi piškotke in ostale informacije.
2. *Po več uspešnih povezovanjih na stran začnem dobivati čudne rezultate, ki so nekoliko podobni prejšnjemu vprašanju.*
Če prepogosto zahtevate podatke s spletne strani, se lahko le-ta odloči, da vas bo blokirala. Dober nasvet (in v splošnem lepa gesta) je, da v pogojih uporabe spletne strani preberete, ali imajo za pajke kakšne omejitve in se jih poskušate držati.
Lahko pa jih nekoliko razbremenite in poskušate prikazati kot da ste človek:
    * Uporabite `User-Agent` iz prejšnjega vprašanja.
    * Nekoliko omejite prenos in med posamezne prenose dodajte čakanje z uporabo `time.sleep`.
    * Počakajte nekaj minut, ali nekaj ur in poskusite ponovno prenesti spletno stran. Pri tem bodite nekoliko bolj natančni in in ponovno prenesite zgolj strani, ki se še niso prenesle, ali pa so se prenesle narobe. Pri analizi podatkov potem poskrbite, da odstranite morebitne ponovitve, dvojne prenose in podobno.

3. *Spletna stran se nalaga dinamično, zato je stran, ki jo prenese knjižnica `requests` brez uporabnih podatkov.*
Vsebina spletne strani se naloži dinamično z uporabo javascripta (ali pa wasm), to pomeni, da morate vložiti nekoliko več dal, da boste lahko prišli podatkov. Tak primer je recimo [cobiss](https://plus.cobiss.si/most-read-web/#libAcronym=MKL&libType&materialTypes=7&periodFrom=201910&periodTo=202009&pubType=1&publishYear).
   1. Najprej morate ugotoviti, na katerem naslovu so na voljo podatki, ki jih spletna stran naloži dinamično.
   To najlažje storite tako, da v brskalniku (Chrome ali Mozilla Firefox) odprete razvojna orodja (Developer tools) in pogledate pod zavihek omrežje.
   Ponovno naložite stran (ali kliknite na gum išči) in v rezultatih poiščite tistega, ki ima željene podatke. Tipično (a ne vedno!) je to kakšen klic na `json`. Navadno lahko brez težav ignorirate `.css` datoteke in zahteve po slikah.
   2. Pozorno preglejte zahtevo (request) in odgovor (response) in poskušajte izluščiti, na kateri naslov in na kakšen način se podatki zahtevajo. Preglejte tudi v kakšne formatu podatke dobite nazaj.
   3. S knjižnjico requests poustvarite ta zahtevek. Navadno morate dodati tudi nekaj avtentikacije (pri tem si lahko pomagate s sejo) in dodatne parametre.
   4. Če spletna stran zahtevek pošlje kot `json` je običajno treba nastaviti `Content-Type` saj bo drugače prišlo do napake, ali pa že kar podatke pošljete kot `json`: `post(url, json=data, headers=headers)`.

4. *Spletna stran se osvežuje dinamično, zahteva, ki jo najdem v razvijalskih orodjih pa je čudna in nerazumljiva*
Ena izmed možnosti je da spletna stran uporablja `graphql`. To je sicer učinkovit in zelo uporaben format, ki si ga po želji lahko pogledate. Težavnost v tem primeru najbrž presega pričakovano zahtevnost projekta pri predmetu.
Druga možnost je, da spletna stran za komunikacijo uporablja popolnoma specifičen format, v tem primeru se boste žal morali močno poglobiti v format, ali pa zamenjati spletno stran.
V skrajni sili lahko pogledate uporabo orodja [Selenium](https://www.selenium.dev/), ki pa ga razen v skrajni sili asistenti in profesor močno odsvetujejo.
