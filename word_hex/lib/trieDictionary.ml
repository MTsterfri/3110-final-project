type t = (int, string) Hashtbl.t

let contains str d : bool =
  try
    let _ = Hashtbl.find d (Hashtbl.hash (String.lowercase_ascii str)) in
    true
  with Not_found -> false

let find str d = Hashtbl.find_opt d (Hashtbl.hash (String.lowercase_ascii str))

let insert str d =
  let strl = String.lowercase_ascii str in
  Hashtbl.add d (Hashtbl.hash strl) strl

let to_list d =
  d |> Hashtbl.to_seq |> List.of_seq |> List.split |> snd
  |> List.sort String.compare

let of_list lst =
  let tbl = Hashtbl.create (List.length lst) in
  let rec add (l : string list) =
    match l with
    | [] -> tbl
    | h :: t ->
        let _ = insert h tbl in
        add t
  in
  add lst

let remove str d = failwith "Unimplimented"
