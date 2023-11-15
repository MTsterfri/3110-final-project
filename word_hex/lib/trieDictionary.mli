(* The signature of a mutable word dictionary implmented as a trie. *)
type t
(** Type representing the data in the dictionary. *)

val to_list : t -> string list
(** [build d] takes a dictionary [d] and returns its elements as a list of words
    sorted in increasing alphabetical order. All words are lowercase*)

val of_list : string list -> t
(** [build lst] takes a list [ls] of string words and makes them into a
    dictionary.*)

val contains : string -> t -> bool
(** [contains str d] returns [true] iff [str] is bound in [d] regardless of
    character case.*)

val find : string -> t -> string option
(** [find str d] is [Some lowercase_str] if [str] is bound in [d] regardless of
    character case; or if [str] is not bound, then it is [None].The returned
    string is lowrcase. *)

val insert : string -> t -> unit
(** [insert str t] inserts the word [str] in lowercase into a dictionary [t]. If
    the dicitonary already contains [str] changes nothing. *)

val remove : string -> t -> unit
(** [remove str t] removes the word [str] from a dictionary [t] regardless of
    case. If a dictionary does not contain the word, it makes no changes and
    returns [None]*)
