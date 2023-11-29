open Dictionary

module Make = struct
  type t = string list

  let empty = []
  let to_list (lst : t) : string list = List.sort String.compare lst

  let of_list (lst : string list) : t =
    let l = List.map (fun x -> String.lowercase_ascii x) lst in
    let check xs x = if List.mem x xs then xs else x :: xs in
    let remove_duplicates xs = List.rev (List.fold_left check [] xs) in
    remove_duplicates l

  let rec contains (str : string) (dict : t) : bool =
    let ls = String.lowercase_ascii str in
    match dict with
    | [] -> false
    | h :: l -> if ls = h then true else contains ls (of_list l)

  let insert (str : string) (dict : t) : t =
    if contains str dict then dict else String.lowercase_ascii str :: dict

  let rec find (str : string) (dict : t) : string option =
    let ls = String.lowercase_ascii str in
    match dict with
    | [] -> None
    | h :: l -> if ls = h then Some ls else find ls (of_list l)

  let rec remove (str : string) (dict : t) : t =
    let ls = String.lowercase_ascii str in
    match dict with
    | [] -> dict
    | h :: l ->
        if ls = h then remove ls (of_list l) else h :: remove ls (of_list l)

  let of_char_list lst dict : string list =
    let out =
      List.filter
        (fun x ->
          let ex = Util.expand x in
          let rec f l =
            match l with
            | [] -> true
            | h :: t -> if List.mem h lst then f t else false
          in
          f ex)
        dict
    in
    to_list out
end
