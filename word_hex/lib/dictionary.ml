(** The signature of a word dictionary. *)
module type DictionaryType = sig
  type t

  val to_list : t -> string list
  val of_list : string list -> t
  val contains : string -> t -> bool
  val insert : string -> t -> unit
  val remove : string -> t -> unit
end

module HashDict : DictionaryType = struct
  type t = (int, string) Hashtbl.t

  let contains (str : string) (dict : t) : bool =
    match Hashtbl.find_opt dict (Hashtbl.hash str) with
    | None -> false
    | Some _ -> true

  let insert (str : string) (dict : t) : unit =
    if contains str dict then ()
    else
      let strl = String.lowercase_ascii str in
      Hashtbl.add dict (Hashtbl.hash strl) strl

  let to_list (dict : t) : string list =
    dict |> Hashtbl.to_seq |> List.of_seq |> List.split |> snd
    |> List.sort String.compare

  let of_list (lst : string list) : t =
    let tbl = Hashtbl.create (List.length lst) in
    let rec add (l : string list) =
      match l with
      | [] -> tbl
      | h :: t ->
          insert h tbl;
          add t
    in
    add lst

  let remove (str : string) (dict : t) : unit = failwith "Unimplimented"
end

(* module HashDict : DictionaryType = struct type t = string list

   let to_list (dict : t) : string list = dict let of_list (lst : string list) :
   t = lst

   let rec contains (str : string) (dict : t) : bool = (* match dict with | []
   -> false | h :: l -> if str = h then true else contains str (build l) *) true

   let insert (str : string) (dict : t) : t = str :: dict

   let rec remove (str : string) (dict : t) : t = match dict with | [] -> dict |
   h :: l -> if str = h then remove str (of_list l) else h :: remove str
   (of_list l) end *)
