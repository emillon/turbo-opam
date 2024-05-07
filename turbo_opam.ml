module Let_syntax = struct
  open Cmdliner.Term

  let ( let+ ) x f = const f $ x
end

exception Lexing_error of Lexing.position

let highlight (pos : Lexing.position) =
  let input = Pp_loc.Input.file pos.pos_fname in
  let ppf = Format.std_formatter in
  let location = Pp_loc.Position.of_lexing pos in
  let locs = [ (location, Pp_loc.Position.shift location 1) ] in
  Pp_loc.pp ~input ppf locs

let parse_exp path =
  try
    In_channel.with_open_bin path (fun ic ->
        let lb = Lexing.from_channel ic in
        Lexing.set_filename lb path;
        let continue = ref true in
        while !continue do
          match Lexer.token lb with
          | Some s -> print_endline (Lexer.token_to_string s)
          | None -> continue := false
          | exception Failure _ -> raise (Lexing_error lb.lex_curr_p)
        done)
  with Lexing_error pos ->
    Printf.printf "lexing error near:\n";
    highlight pos;
    exit 1

let term =
  let open Let_syntax in
  let+ path =
    Cmdliner.Arg.required
      (Cmdliner.Arg.pos 0
         Cmdliner.Arg.(some string)
         None (Cmdliner.Arg.info []))
  in
  let _control =
    let filename = OpamFile.make (OpamFilename.of_string path) in
    In_channel.with_open_bin path (OpamFile.OPAM.read_from_channel ~filename)
  in
  let _exp = parse_exp path in
  print_endline path

let info = Cmdliner.Cmd.info "turbo-opam"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
