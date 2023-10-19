(** The signature of a word dictionary. *)
module type DictionaryType = sig
  type t

  val to_list : t -> string list
  val of_list : string list -> t
  val contains : string -> t -> bool
  val insert : string -> t -> t
  val remove : string -> t -> t
end

(* module HashDict : DictionaryType = struct type t = unit

   let build (lst : string list) = ignore lst; failwith "Unimplimented"

   let contains (str : string) (dict : t) : bool = ignore str; ignore dict;
   failwith "Unimplimented"

   let insert (str : string) (dict : t) : t = ignore str; ignore dict; failwith
   "Unimplimented"

   let remove (str : string) (dict : t) : t = ignore str; ignore dict; failwith
   "Unimplimented" end *)

module HashDict : DictionaryType = struct
  type t = string list

  let to_list (dict : t) : string list = dict
  let of_list (lst : string list) : t = lst

  let rec contains (str : string) (dict : t) : bool =
    (* match dict with | [] -> false | h :: l -> if str = h then true else
       contains str (build l) *)
    true

  let insert (str : string) (dict : t) : t = str :: dict

  let rec remove (str : string) (dict : t) : t =
    match dict with
    | [] -> dict
    | h :: l ->
        if str = h then remove str (of_list l) else h :: remove str (of_list l)
end
