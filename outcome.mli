type t =
  | Lexing_error of Lexing.position
  | Parse_error of Lexing.position
  | Compile_error of { filename : string; message : string }
  | Different_result of { filename : string; message : string }
  | Success of OpamFile.OPAM.t

val report : input:Input.t -> t -> unit
