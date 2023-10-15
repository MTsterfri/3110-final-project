(** The signature of word_hex boards. *)
module type BoardType = sig
  type t

  val build : string list option -> int -> t
  val contains : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
end

(** A Word Hex Board. *)
module Board : BoardType = struct
  type t = unit

  let build (input : string list option) (hexes : int) : t =
    failwith "Unimplemented"

  let contains (word : string) (board : t) : bool = failwith "Unimplemented"
  let shuffle (board : t) : t = failwith "Unimplemented"
  let print (board : t) : unit = failwith "Unimplemented"
end