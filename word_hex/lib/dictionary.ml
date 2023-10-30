type t = (int, string) Hashtbl.t

let contains_opt (str : string) (dict : t) : string option =
  Hashtbl.find_opt dict (Hashtbl.hash (String.lowercase_ascii str))

let insert_opt (str : string) (dict : t) : string option =
  match contains_opt str dict with
  | None ->
      let strl = String.lowercase_ascii str in
      Hashtbl.add dict (Hashtbl.hash strl) strl;
      Some strl
  | Some _ -> None

let to_list (dict : t) : string list =
  dict |> Hashtbl.to_seq |> List.of_seq |> List.split |> snd
  |> List.sort String.compare

let of_list (lst : string list) : t =
  let tbl = Hashtbl.create (List.length lst) in
  let rec add (l : string list) =
    match l with
    | [] -> tbl
    | h :: t ->
        let _ = insert_opt h tbl in
        add t
  in
  add lst

let remove_opt (str : string) (dict : t) : string option =
  failwith "Unimplimented"
