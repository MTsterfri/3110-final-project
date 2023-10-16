(** The signature of a word dictionary. *)
module type DictionaryType = sig
  type t
  (** Type representing the data in the dictionary. *)

  val build : string list -> t
  (** [build lst] takes a list of string words and makes them into a dictionary*)

  val contains : string -> t -> bool
  (** [contains str t] returns whether or not a word [str] is contained within
      the dictionary [t]*)

  val insert : string -> t -> t
  (** [insert str t] inserts the word [str] into a dictionary [t]*)

  val remove : string -> t -> t
  (** [remove str t] removes the word [str] from a dictionary [t]. If a
      dictionary does not contain the word, it returns the original dictionary*)
end

module HashDict : DictionaryType
(** Dictionary such that words are stored in a HashMap in order to improve
    efficiency. *)
