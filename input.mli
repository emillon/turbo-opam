type t = File of { path : string } | String of { data : string }

val filename : t -> string
val pp_input : t -> Pp_loc.Input.t
val with_lexbuf : f:(Lexing.lexbuf -> 'a) -> t -> 'a
