type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let test =
  [ Element 1;
    Element 2;
    Podseznam [
      Element 3;
      Podseznam [Element 4];
      Podseznam [];
    ] ;
    Element 5
  ]

let rec najvecja_globina = function
  | [] -> 1
  | Element _ :: xs -> najvecja_globina xs
  | Podseznam podsez :: xs ->
      max (najvecja_globina podsez + 1) (najvecja_globina xs)

let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: preslikaj f xs
  | Podseznam podsez :: xs -> Podseznam (preslikaj f podsez) :: preslikaj f xs

let rec splosci = function
  | [] -> []
  | Element x :: xs -> x :: splosci xs
  | Podseznam podsez :: xs -> splosci podsez @ splosci xs

let rec alternirajoci_konstruktorji = function
  | [] | _ :: [] -> true
  | Element _ :: (Podseznam _ :: xs as ys)
  | Podseznam _ :: (Element _ :: xs as ys) -> alternirajoci_konstruktorji ys
  | Element _ :: Element _ :: _
  | Podseznam _ :: Podseznam _ :: _ -> false

let rec zlozi_preko_gnezdenja f acc gnezdenje =
  (* Napišemo lastno repno rekurzivno funkcijo za združevanje. *)
  let zdruzi xs ys =
    let rec prelozi ys = function
      | [] -> ys
      | x :: xs -> prelozi (x :: ys) xs
    in
    prelozi ys (List.rev xs)
  in
  let rec zlozi f acc = function
    | [] -> acc
    | Element x :: xs -> zlozi f (f acc x) xs
    | Podseznam podsez :: xs -> zlozi f acc (zdruzi podsez xs)
  in
  zlozi f acc gnezdenje
