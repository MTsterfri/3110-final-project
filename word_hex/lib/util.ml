let expand str =
  String.fold_right (fun c acc -> Char.lowercase_ascii c :: acc) str []
