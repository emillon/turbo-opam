module Let_syntax = struct
  open Cmdliner.Term

  let pair x y = (x, y)
  let ( let+ ) x f = const f $ x
  let ( and+ ) tx ty = const pair $ tx $ ty
end

let highlight (pos : Lexing.position) =
  let input = Pp_loc.Input.file pos.pos_fname in
  let ppf = Format.std_formatter in
  let location = Pp_loc.Position.of_lexing pos in
  let locs = [ (location, Pp_loc.Position.shift location 1) ] in
  Pp_loc.pp ~input ppf locs

type outcome =
  | Lexing_error of Lexing.position
  | Parse_error of Lexing.position
  | Ok

let report_outcome = function
  | Ok -> ()
  | Lexing_error pos ->
      Printf.printf "lexing error near:\n";
      highlight pos;
      exit 1
  | Parse_error pos ->
      Printf.printf "parse error in %s near:\n" pos.pos_fname;
      highlight pos;
      exit 1

let parse_exp ~fail path =
  let exception Lexing_error of Lexing.position in
  let r =
    In_channel.with_open_bin path (fun ic ->
        let lb = Lexing.from_channel ic in
        Lexing.set_filename lb path;
        try
          let _ast =
            Parser.main
              (fun lb ->
                match Lexer.token lb with
                | t -> t
                | exception Failure _ -> raise (Lexing_error lb.lex_curr_p))
              lb
          in
          Ok
        with
        | Parser.Error -> Parse_error lb.lex_curr_p
        | Lexing_error p -> Lexing_error p)
  in
  if fail then report_outcome r;
  r

let iter_on_opam_files root ~f =
  let rec go dir =
    let contents =
      Sys.readdir dir |> Array.to_list |> List.sort String.compare
    in
    List.iter
      (fun part ->
        let path = Filename.concat dir part in
        let stat = Unix.stat path in
        match stat.st_kind with
        | S_REG -> if String.equal part "opam" then f path
        | S_DIR -> go path
        | _ -> assert false)
      contents
  in
  go root

module Outcome_type = struct
  type t = Ok | Lexing_error | Parse_error

  let pp ppf = function
    | Ok -> Format.fprintf ppf "Ok"
    | Lexing_error -> Format.fprintf ppf "Lexing error"
    | Parse_error -> Format.fprintf ppf "Parse error"

  let compare = Stdlib.compare

  let for_outcome : outcome -> t = function
    | Lexing_error _ -> Lexing_error
    | Ok -> Ok
    | Parse_error _ -> Parse_error
end

module Stats = struct
  module M = Map.Make (Outcome_type)

  let empty = M.empty

  let add m o =
    let key = Outcome_type.for_outcome o in
    let f = function None -> Some 1 | Some n -> Some (n + 1) in
    M.update key f m

  let pp ppf t =
    M.iter (fun o n -> Format.fprintf ppf "%a: %d\n" Outcome_type.pp o n) t
end

let term =
  let open Let_syntax in
  let+ repo_path = Cmdliner.Arg.(required & pos 0 (some string) None & info [])
  and+ fail =
    Cmdliner.Arg.value (Cmdliner.Arg.flag (Cmdliner.Arg.info [ "fail" ]))
  in
  let stats = ref Stats.empty in
  iter_on_opam_files repo_path ~f:(fun path ->
      (*let _control =*)
      (*let filename = OpamFile.make (OpamFilename.of_string path) in*)
      (*In_channel.with_open_bin path*)
      (*(OpamFile.OPAM.read_from_channel ~filename)*)
      (*in*)
      let r_exp = parse_exp ~fail path in
      stats := Stats.add !stats r_exp;
      if false then print_endline path);
  Format.printf "%a" Stats.pp !stats

let info = Cmdliner.Cmd.info "turbo-opam"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
