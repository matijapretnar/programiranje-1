let razsiri_lzw najmanjsa_velikost podatki stevilo_pikslov =
  let izhod = Array.make stevilo_pikslov 0 and zapisanih = ref 0 in
  let polozaj = ref 0 in
  let preberi velikost =
    let koda = ref 0 in
    for k = 0 to velikost - 1 do
      let bit = !polozaj + k in
      if Char.code podatki.[bit / 8] land (1 lsl (bit mod 8)) <> 0 then
        koda := !koda lor (1 lsl k)
    done;
    polozaj := !polozaj + velikost;
    !koda
  in
  let pocisti = 1 lsl najmanjsa_velikost in
  let konec = pocisti + 1 in
  let slovar = Array.init 4096 (fun i -> [| i |]) in
  let naslednja = ref (konec + 1) and velikost = ref (najmanjsa_velikost + 1) in
  let prejsnji = ref [||] in
  let nadaljuj = ref true in
  while !nadaljuj && !zapisanih < stevilo_pikslov do
    let koda = preberi !velikost in
    if koda = pocisti then (
      naslednja := konec + 1;
      velikost := najmanjsa_velikost + 1;
      prejsnji := [||])
    else if koda = konec then nadaljuj := false
    else
      let zaporedje =
        if koda < !naslednja then slovar.(koda)
        else Array.append !prejsnji [| !prejsnji.(0) |]
      in
      Array.iter
        (fun i ->
          if !zapisanih < stevilo_pikslov then (
            izhod.(!zapisanih) <- i;
            incr zapisanih))
        zaporedje;
      if !prejsnji <> [||] && !naslednja < 4096 then (
        slovar.(!naslednja) <- Array.append !prejsnji [| zaporedje.(0) |];
        incr naslednja;
        if !naslednja = 1 lsl !velikost && !velikost < 12 then incr velikost);
      prejsnji := zaporedje
  done;
  izhod

let preberi_gif ime_datoteke =
  let vsebina = In_channel.with_open_bin ime_datoteke In_channel.input_all in
  let bajt k = Char.code vsebina.[k] in
  let dva_bajta k = bajt k lor (bajt (k + 1) lsl 8) in
  let paleta zacetek velikost =
    Array.init velikost (fun i ->
        let k = zacetek + (3 * i) in
        (bajt k, bajt (k + 1), bajt (k + 2)))
  in
  let rec preskoci_bloke k = if bajt k = 0 then k + 1 else preskoci_bloke (k + 1 + bajt k) in
  let rec zberi_bloke k bloki =
    if bajt k = 0 then String.concat "" (List.rev bloki)
    else zberi_bloke (k + 1 + bajt k) (String.sub vsebina (k + 1) (bajt k) :: bloki)
  in
  let zastavice = bajt 10 in
  let globalna = paleta 13 (2 lsl (zastavice land 7)) in
  let rec slika k =
    match bajt k with
    | 0x21 -> slika (preskoci_bloke (k + 2))
    | 0x2C ->
        let sirina = dva_bajta (k + 5) and visina = dva_bajta (k + 7) in
        let zastavice = bajt (k + 9) in
        if zastavice land 0x40 <> 0 then failwith "preberi_gif: prepletena slika";
        let barve, k =
          if zastavice land 0x80 = 0 then (globalna, k + 10)
          else
            let velikost = 2 lsl (zastavice land 7) in
            (paleta (k + 10) velikost, k + 10 + (3 * velikost))
        in
        let indeksi = razsiri_lzw (bajt k) (zberi_bloke (k + 1) []) (sirina * visina) in
        Array.init visina (fun i -> Array.init sirina (fun j -> barve.(indeksi.((i * sirina) + j))))
    | _ -> failwith "preberi_gif: datoteka nima slike"
  in
  slika (if zastavice land 0x80 = 0 then 13 else 13 + (3 * Array.length globalna))

let lzw indeksi =
  let izhod = Buffer.create 4096 in
  let zbiralnik = ref 0 and stevilo_bitov = ref 0 in
  let zapisi koda velikost =
    zbiralnik := !zbiralnik lor (koda lsl !stevilo_bitov);
    stevilo_bitov := !stevilo_bitov + velikost;
    while !stevilo_bitov >= 8 do
      Buffer.add_char izhod (Char.chr (!zbiralnik land 0xFF));
      zbiralnik := !zbiralnik lsr 8;
      stevilo_bitov := !stevilo_bitov - 8
    done
  in
  let pocisti = 256 and konec = 257 in
  let slovar = Hashtbl.create 4096 in
  let naslednja = ref 258 and velikost = ref 9 in
  zapisi pocisti !velikost;
  let predpona = ref indeksi.(0) in
  for k = 1 to Array.length indeksi - 1 do
    let c = indeksi.(k) in
    match Hashtbl.find_opt slovar ((!predpona lsl 8) lor c) with
    | Some koda -> predpona := koda
    | None ->
        zapisi !predpona !velikost;
        if !naslednja >= 4095 then (
          zapisi pocisti !velikost;
          Hashtbl.reset slovar;
          naslednja := 258;
          velikost := 9)
        else (
          Hashtbl.add slovar ((!predpona lsl 8) lor c) !naslednja;
          incr naslednja;
          if !naslednja > 1 lsl !velikost && !velikost < 12 then incr velikost);
        predpona := c
  done;
  zapisi !predpona !velikost;
  zapisi konec !velikost;
  if !stevilo_bitov > 0 then Buffer.add_char izhod (Char.chr !zbiralnik);
  Buffer.contents izhod

let shrani_gif ime_datoteke slike =
  let visina = Array.length (List.hd slike) in
  let sirina = List.fold_left (fun m s -> max m (Array.length s.(0))) 0 slike in
  let bela = (255, 255, 255) in
  let paleta = Hashtbl.create 256 in
  let indeks ((r, g, m) as barva) =
    let kljuc = (r lsl 16) lor (g lsl 8) lor m in
    match Hashtbl.find_opt paleta kljuc with
    | Some (i, _) -> i
    | None ->
        let i = Hashtbl.length paleta in
        Hashtbl.add paleta kljuc (i, barva);
        i
  in
  let okvir slika =
    Array.init (visina * sirina) (fun k ->
        let vrstica = slika.(k / sirina) and j = k mod sirina in
        indeks (if j < Array.length vrstica then vrstica.(j) else bela))
  in
  let okvirji = List.map okvir slike in
  if Hashtbl.length paleta > 256 then invalid_arg "shrani_gif: več kot 256 barv";
  let barve = Array.make 256 bela in
  Hashtbl.iter (fun _ (i, barva) -> barve.(i) <- barva) paleta;
  let b = Buffer.create 1_000_000 in
  let bajt x = Buffer.add_char b (Char.chr (x land 0xFF)) in
  let dva_bajta x = bajt x; bajt (x lsr 8) in
  Buffer.add_string b "GIF89a";
  dva_bajta sirina; dva_bajta visina; bajt 0xF7; bajt 0; bajt 0;
  Array.iter (fun (r, g, m) -> bajt r; bajt g; bajt m) barve;
  Buffer.add_string b "\x21\xFF\x0BNETSCAPE2.0\x03\x01\x00\x00\x00";
  let zadnji = List.length okvirji - 1 in
  List.iteri
    (fun n indeksi ->
      Buffer.add_string b "\x21\xF9\x04\x00";
      dva_bajta (if n = zadnji then 300 else 4); bajt 0; bajt 0;
      bajt 0x2C; dva_bajta 0; dva_bajta 0; dva_bajta sirina; dva_bajta visina; bajt 0;
      bajt 8;
      let podatki = lzw indeksi in
      let rec bloki i =
        if i < String.length podatki then (
          let k = min 255 (String.length podatki - i) in
          bajt k;
          Buffer.add_substring b podatki i k;
          bloki (i + k))
      in
      bloki 0;
      bajt 0)
    okvirji;
  bajt 0x3B;
  Out_channel.with_open_bin ime_datoteke (fun kanal ->
      Out_channel.output_string kanal (Buffer.contents b))
