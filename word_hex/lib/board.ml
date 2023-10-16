(** The signature of word_hex boards. *)
module type BoardType = sig
  type t

  val build : string list option -> t
  val contains : string -> t -> bool
  val shuffle : t -> t
  val print : t -> unit
end

type hex = {
  center : char;
  h0 : char;
  h1 : char;
  h2 : char;
  h3 : char;
  h4 : char;
  h5 : char;
}
(** Type representing a single hex of a board (6 letters in a hexagon pattern
    around a 7th centeral letter)*)

(** Type representing the 7 positions on a hex. Center is the central letter,
    while Hn for n in (0..5) inclusive goes around the hexagon clockwise with H0
    starting at 12:00. *)
type hex_pos =
  | Center
  | H0
  | H1
  | H2
  | H3
  | H4
  | H5

(** Association List for uppercase english vowels*)
let vowel_list = [ (0, 'A'); (1, 'E'); (2, 'I'); (3, 'O'); (4, 'U') ]

(** Association List for uppercase english consonants*)
let consonant_list =
  [
    (0, 'B');
    (1, 'C');
    (2, 'D');
    (3, 'F');
    (4, 'G');
    (5, 'H');
    (6, 'J');
    (7, 'K');
    (8, 'K');
    (9, 'M');
    (10, 'N');
    (11, 'P');
    (12, 'Q');
    (13, 'R');
    (14, 'S');
    (15, 'T');
    (16, 'V');
    (17, 'W');
    (18, 'X');
    (19, 'Y');
    (20, 'Z');
  ]

(** Picks n random elements from lst. Ensures no two elements in the list
    returned come from the same 'index' of the list. This means, if all elements
    of lst are unique, all elements of the returned list will also be unique.
    Requires 0 <= n <= List.length lst*)
let rec pick_random (lst : ('a * 'b) list) (n : int) : 'b list =
  let len = List.length lst in
  if n == 0 then []
  else
    let key, pick = List.nth lst (Random.int len) in
    pick :: pick_random (List.remove_assoc key lst) (n - 1)

(** Randomizes the order of lst and appends result to acc*)
let rec randomize (acc : 'a list) (lst : 'a list) : 'a list =
  let len = List.length lst in
  match lst with
  | [] -> acc
  | _ ->
      let next = List.nth lst (Random.int len) in
      randomize (acc @ [ next ]) (List.filter (fun x -> x != next) lst)

(** Returns if the given hex contains the word. The given hex contains the word
    all characters of the word are found within the hex*)
let rec hex_contains (h : hex) (word : string) : bool =
  let hlst = [ h.center; h.h0; h.h1; h.h2; h.h3; h.h4; h.h5 ] in
  String.fold_left
    (fun bl chr -> bl && List.exists (fun lst_chr -> lst_chr = chr) hlst)
    true word

(** A Word Hex Board. *)
module HexBoard : BoardType = struct
  type t = hex

  let build (input : string list option) : t =
    ignore input;
    let vowels = pick_random vowel_list 2 in
    let consonants = pick_random consonant_list 5 in
    let outer = randomize [] (List.nth vowels 1 :: consonants) in
    {
      center = List.nth vowels 0;
      h0 = List.nth outer 0;
      h1 = List.nth outer 1;
      h2 = List.nth outer 2;
      h3 = List.nth outer 3;
      h4 = List.nth outer 4;
      h5 = List.nth outer 5;
    }

  let contains (word : string) (board : t) : bool =
    hex_contains board (String.uppercase_ascii word)

  let shuffle (board : t) : t =
    let outer =
      randomize []
        [ board.h0; board.h1; board.h2; board.h3; board.h4; board.h5 ]
    in
    {
      center = board.center;
      h0 = List.nth outer 0;
      h1 = List.nth outer 1;
      h2 = List.nth outer 2;
      h3 = List.nth outer 3;
      h4 = List.nth outer 4;
      h5 = List.nth outer 5;
    }

  let print (board : t) : unit =
    ignore board;
    failwith "Unimplemented"
end
