let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let ( and+ ) x y =
  let* x in
  let+ y in
  (x, y)
