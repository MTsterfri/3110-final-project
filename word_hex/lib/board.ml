(** The signature of word_hex boards. *)
module type BoardType = sig
  type t

  val build : string list option -> t
  val contains : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
end

type hex = {
  center : string;
  h0 : string;
  h1 : string;
  h2 : string;
  h3 : string;
  h4 : string;
  h5 : string;
}
(** Type representing a single hex of a board (6 letters in a hexagon pattern
    around a 7th centeral letter)*)

type hex_pos =
  | Center
  | H0
  | H1
  | H2
  | H3
  | H4
  | H5

(** Type representing the 7 positions on a hex. Center is the central letter,
    while Hn for n in (0..5) inclusive goes around the hexagon clockwise with H0
    starting at 12:00. *)

(** A Word Hex Board. *)
module HexBoard : BoardType = struct
  type t = hex

  let build (input : string list option) : t =
    ignore input;
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
