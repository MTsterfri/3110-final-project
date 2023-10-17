open Board
open Dictionary

(** The signature of a word_hex game. *)
module type GameType = sig
  type t

  val build : string list option -> HashDict.t -> t
  val update : t -> string -> t
  val found : t -> string list
  val print : t -> unit
end

module Game (Board : BoardType) : GameType = struct
  module Board : BoardType = Board

  type t = {
    score : int;
    found_words : string list;
    board : Board.t;
    dictionary : HashDict.t;
    message : string;
  }

  let build (words : string list option) (dict : HashDict.t) : t =
    {
      score = 0;
      found_words = [];
      board = Board.build words;
      dictionary = dict;
      message = "";
    }

  let update (game : t) (word : string) : t = failwith "Unimplemented"
  let found (game : t) : string list = failwith "Unimplemented"
  let print (game : t) : unit = failwith "Unimplemented"
end
