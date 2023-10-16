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
  type hex_pos =
    | Center
    | H0
    | H1
    | H2
    | H3
    | H4
    | H5

  type hex = {
    center : string;
    h0 : string;
    h1 : string;
    h2 : string;
    h3 : string;
    h4 : string;
    h5 : string;
  }

  type t = {
    hexes : hex list;
    num_hex : int;
    overlaps : (hex_pos * int) list list;
  }

  let build (input : string list option) (hexes : int) : t =
    ignore input;
    ignore hexes;
    failwith "Unimplemented"

  let contains (word : string) (board : t) : bool =
    ignore word;
    ignore board;
    failwith "Unimplemented"

  let shuffle (board : t) : t =
    ignore board;
    failwith "Unimplemented"

  let print (board : t) : unit =
    ignore board;
    failwith "Unimplemented"
end
