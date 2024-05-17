type t =
  | Lexing_error of Lexing.position
  | Parse_error of Lexing.position
  | Compile_error of { filename : string; message : string }
  | Different_result of { filename : string; message : string }
  | Success of OpamFile.OPAM.t

let highlight ~input (pos : Lexing.position) =
  let pp_input =
    Input.pp_input input
    (*match input with*)
    (*| Some s -> Pp_loc.Input.string s*)
    (*| None -> Pp_loc.Input.file pos.pos_fname*)
  in
  let ppf = Format.std_formatter in
  let location = Pp_loc.Position.of_lexing pos in
  let locs = [ (location, Pp_loc.Position.shift location 1) ] in
  Pp_loc.pp ~input:pp_input ppf locs

let report ~input ~fatal t =
  let error () = if fatal then exit 1 in
  match t with
  | Success _ -> ()
  | Different_result { filename; message } ->
      Printf.printf "different result for %s: %s\n" filename message;
      error ()
  | Lexing_error pos ->
      Printf.printf "lexing error near:\n";
      highlight ~input pos;
      error ()
  | Parse_error pos ->
      Printf.printf "parse error in %s near:\n" pos.pos_fname;
      highlight ~input pos;
      error ()
  | Compile_error { filename; message } ->
      Printf.printf "compile error in %s: %s\n" filename message;
      error ()
