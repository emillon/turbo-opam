let errorf fmt = Format.kasprintf Result.error fmt

let traverse l =
  let open Result_let_syntax in
  List.fold_right
    (fun x xs ->
      let+ x and+ xs in
      x :: xs)
    l (Ok [])
