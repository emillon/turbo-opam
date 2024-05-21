let errorf fmt = Format.kasprintf Result.error fmt

let map_m ~f l =
  let open Result_let_syntax in
  List.fold_right
    (fun x xs ->
      let+ y = f x and+ ys = xs in
      y :: ys)
    l (Ok [])
