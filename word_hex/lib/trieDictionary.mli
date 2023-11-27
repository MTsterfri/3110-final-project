(* The signature of a word dictionary implmented as a trie. *)
type t
(** Type representing the data in the dictionary. *)

val empty : t
(** [empty] creates a dictionary that contians zero words and hold no elements*)

val insert : string -> t -> t
(** [insert str d] inserts the word [str] in lowercase into a dictionary [d]. If
    the dicitonary already contains [str] changes nothing. *)

val to_list : t -> string list
(** [to_list d] takes a dictionary [d] and returns its elements as a list of
    words sorted in increasing alphabetical order. All words are lowercase*)

val of_list : string list -> t
(** [of_list lst] takes a list [lst] of string words and makes them into a
    dictionary.*)

val contains : string -> t -> bool
(** [contains str d] returns [true] iff [str] is bound in [d] regardless of
    character case. Returns [true] for the empty string*)

val find : string -> t -> string option
(** [find str d] is [Some lowercase_str] if [str] is bound in [d] regardless of
    character case; or if [str] is not bound, then it is [None].The returned
    string is lowrcase. Returns [true] for the empty string*)

val remove : string -> t -> t
(** [remove str d] removes the word [str] from a dictionary [d] regardless of
    case. If a dictionary does not contain the word, it makes no changes*)
