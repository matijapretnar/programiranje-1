type 'a merkle = List | Vozlisce of 'a vozlisce

and 'a vozlisce = {
  levo : 'a merkle;
  podatek : 'a;
  desno : 'a merkle;
  zgostitev : int;
}

type 'a hash = int -> 'a -> int -> int

let primer_h l p d = ((l * 3) + (p * 5) + (d * 7)) mod 11

let drevo : int merkle =
  Vozlisce
    {
      levo =
        Vozlisce
          {
            levo =
              Vozlisce
                { levo = List; podatek = 10; desno = List; zgostitev = 6 };
            podatek = 14;
            desno =
              Vozlisce
                { levo = List; podatek = 474; desno = List; zgostitev = 5 };
            zgostitev = 2;
          };
      podatek = 57;
      desno =
        Vozlisce
          {
            levo = List;
            podatek = 12;
            desno =
              Vozlisce
                { levo = List; podatek = 513; desno = List; zgostitev = 2 };
            zgostitev = 8;
          };
      zgostitev = 6;
    }

let zgosti = function List -> 0 | Vozlisce vozlisce -> vozlisce.zgostitev

let rec popravi h = function
  | List -> List
  | Vozlisce vozlisce -> Vozlisce (popravi_vozlisce h vozlisce)

and popravi_vozlisce h vozlisce =
  let levo = popravi h vozlisce.levo
  and podatek = vozlisce.podatek
  and desno = popravi h vozlisce.desno in
  let zgostitev = h (zgosti levo) podatek (zgosti desno) in
  { levo; podatek; desno; zgostitev }

let rec prestej_napacne h = function
  | List -> 0
  | Vozlisce vozlisce ->
      let prava_zgostitev =
        vozlisce.zgostitev
        = h (zgosti vozlisce.levo) vozlisce.podatek (zgosti vozlisce.desno)
      in
      prestej_napacne h vozlisce.levo
      + prestej_napacne h vozlisce.desno
      + if prava_zgostitev then 0 else 1

let preveri h d = prestej_napacne h d = 0
