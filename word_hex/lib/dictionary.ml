(** The signature of a word dictionary. *)
module type DictionaryType = sig
  type t

  val build : string list -> t
  val contains : string -> t -> bool
  val insert : string -> t -> t
  val remove : string -> t -> t
end

module HashDict : DictionaryType = struct
  type t = unit

  let build (lst : string list) =
    ignore lst;
    failwith "Unimplimented"

  let contains (str : string) (dict : t) : bool =
    ignore str;
    ignore dict;
    failwith "Unimplimented"

  let insert (str : string) (dict : t) : t =
    ignore str;
    ignore dict;
    failwith "Unimplimented"

  let remove (str : string) (dict : t) : t =
    ignore str;
    ignore dict;
    failwith "Unimplimented"
end
