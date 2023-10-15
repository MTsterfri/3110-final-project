(** The signature of a word dictionary. *)
module type DicitonaryType = sig

  (** Type representing the data in the dictionary. *)
  type 'a t
end

(** Dictionary such that words are stored in a HashMap in order to improve efficiency. *)
module HashDict : DicitonaryType