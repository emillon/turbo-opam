type t = File of { path : string } | String of { data : string }

let filename = function File { path } -> path | String _ -> "string.0.opam"

let with_lexbuf ~f = function
  | File { path } as t ->
      In_channel.with_open_bin path (fun ic ->
          let lb = Lexing.from_channel ic in
          Lexing.set_filename lb (filename t);
          f lb)
  | String { data } as t ->
      let lb = Lexing.from_string data in
      Lexing.set_filename lb (filename t);
      f lb

let pp_input = function
  | File { path } -> Pp_loc.Input.file path
  | String { data } -> Pp_loc.Input.string data
