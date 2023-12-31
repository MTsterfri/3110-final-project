open Dictionary

module Make = struct
  module CharMap = Map.Make (Char)

  type t = Node of string option * t CharMap.t

  let empty = Node (None, CharMap.empty)

  let insert str d : t =
    match String.lowercase_ascii str with
    | "" -> d
    | s ->
        let rec ins ls (Node (v, m)) =
          match ls with
          | [] -> Node (Some s, m)
          | h :: t -> (
              match CharMap.find_opt h m with
              | None -> Node (v, CharMap.add h (ins t empty) m)
              | Some n -> Node (v, CharMap.update h (fun _ -> Some (ins t n)) m)
              )
        in
        ins (Util.expand s) d

  let rec to_list (d : t) =
    match d with
    | Node (None, m) -> CharMap.fold (fun _ d2 acc -> acc @ to_list d2) m []
    | Node (Some str, m) ->
        str :: CharMap.fold (fun _ d2 acc -> acc @ to_list d2) m []

  let of_list lst = List.fold_left (fun acc x -> insert x acc) empty lst

  let contains str d : bool =
    match String.lowercase_ascii str with
    | "" -> true
    | s ->
        let rec con ls (Node (v, m)) =
          match ls with
          | [] -> v = Some s
          | h :: t -> (
              match CharMap.find_opt h m with
              | None -> false
              | Some n -> con t n)
        in
        con (Util.expand s) d

  let find str d : string option =
    match String.lowercase_ascii str with
    | "" -> Some ""
    | s ->
        let rec f ls (Node (v, m)) =
          match ls with
          | [] -> if v = Some s then Some s else None
          | h :: t -> (
              match CharMap.find_opt h m with
              | None -> None
              | Some n -> f t n)
        in
        f (Util.expand s) d

  let remove str d : t =
    match String.lowercase_ascii str with
    | "" -> d
    | s ->
        let rec rem ls (Node (v, m)) =
          match ls with
          | [] -> Node (None, m)
          | h :: t -> (
              match CharMap.find_opt h m with
              | None -> d
              | Some n -> Node (v, CharMap.update h (fun _ -> Some (rem t n)) m)
              )
        in
        rem (Util.expand s) d

  let of_char_list lst dict : string list =
    (* let n = match dict with | Node (v, m) -> Node (v, CharMap.filter (fun k _
       -> List.mem k lst) m) in match n with | Node (v, m) -> assert false *)
    let rec alt d =
      match d with
      | Node (n, m) ->
          Node
            ( n,
              CharMap.filter_map
                (fun k v -> if List.mem k lst then Some (alt v) else None)
                m )
    in
    dict |> alt |> to_list
  (* TODO *)
end
