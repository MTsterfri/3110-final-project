open Board
open Dictionary

(** The signature of a word_hex game. *)
module type GameType = sig
  type t

  val build : string list option -> int -> t
  val update : t -> string -> t
  val found : t -> string list
  val print : t -> unit
end

module Game (Board : BoardType) (Dictionary : DictionaryType) : GameType =
struct
  (* TODO: Implement this type and module *)
  type t = unit

  let build (words : string list option) (hexes : int) : t =
    failwith "Unimplemented"

  let update (game : t) (word : string) : t = failwith "Unimplemented"
  let found (game : t) : string list = failwith "Unimplemented"
  let print (game : t) : unit = failwith "Unimplemented"
end
