type t =
  | Success
  | Lexing_error
  | Parse_error
  | Compile_error
  | Different_result

let pp ppf = function
  | Success -> Format.fprintf ppf "Success"
  | Lexing_error -> Format.fprintf ppf "Lexing error"
  | Parse_error -> Format.fprintf ppf "Parse error"
  | Compile_error -> Format.fprintf ppf "Compile error"
  | Different_result -> Format.fprintf ppf "Different result"

let compare = Stdlib.compare

let for_outcome : Outcome.t -> t = function
  | Lexing_error _ -> Lexing_error
  | Success _ -> Success
  | Parse_error _ -> Parse_error
  | Compile_error _ -> Compile_error
  | Different_result _ -> Different_result
