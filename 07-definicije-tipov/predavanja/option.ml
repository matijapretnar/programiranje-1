let glava =
  function
  | x :: xs -> x
  | [] -> failwith "Ups."

let ima_veliko_glavo xs = glava xs > 100

type 'a morebitni  =
  | Nimamo
  | Imamo of 'a

(* type 'a option =
  | None
  | Some of 'a *)

let varna_glava =
  function
  | x :: xs -> Some x
  | [] -> None

;;

let ima_varno_veliko_glavo xs =
  match varna_glava xs with
  | Some g -> Some (g > 100)
  | None -> None