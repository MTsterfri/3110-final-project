module type Dictionary = sig
  type t

  val empty : t
  val insert : string -> t -> t
  val to_list : t -> string list
  val of_list : string list -> t
  val contains : string -> t -> bool
  val find : string -> t -> string option
  val remove : string -> t -> t
  val of_char_list : char list -> t -> string list
end
