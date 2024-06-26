module Let_syntax = struct
  open Cmdliner.Term

  let pair x y = (x, y)
  let ( let+ ) x f = const f $ x
  let ( and+ ) tx ty = const pair $ tx $ ty
end

(*type 'a formula =*)
(*| Empty*)
(*| Atom of 'a*)
(*| Block of 'a formula*)
(*| And of 'a formula * 'a formula*)
(*| Or of 'a formula * 'a formula*)
(*type filter =*)
(*type 'a filter_or_constraint =*)
(*| Filter of filter*)
(*| Constraint of (relop * 'a)*)
(*type filtered_formula =*)
(*(name * filter filter_or_constraint OpamFormula.formula) OpamFormula.formula*)

let token_to_string : Parser.token -> _ = function
  | String _ -> "String"
  | Rparen -> "Rparen"
  | Rbracket -> "Rbracket"
  | Rbrace -> "Rbrace"
  | PlusEq -> "PlusEq"
  | Or -> "Or"
  | Not -> "Not"
  | Neq -> "Neq"
  | Lt -> "Lt"
  | Lparen -> "Lparen"
  | Le -> "Le"
  | Lbracket -> "Lbracket"
  | Lbrace -> "Lbrace"
  | Ident _ -> "Ident"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Eq -> "Eq"
  | Eof -> "Eof"
  | Colon -> "Colon"
  | And -> "And"

let parse_lb ~debug_tokens ~debug_ast lb =
  let exception Lexing_error of Lexing.position in
  try
    let ast =
      Parser.main
        (fun lb ->
          match Lexer.token lb with
          | t ->
              if debug_tokens then Printf.printf "%s\n" (token_to_string t);
              t
          | exception Failure _ -> raise (Lexing_error lb.lex_curr_p))
        lb
    in
    if debug_ast then Format.printf "%a\n" Ast.pp ast;
    match Compile.compile ast with
    | Ok r -> Outcome.Success r
    | Error message -> Compile_error { filename = ast.filename; message }
  with
  | Parser.Error -> Parse_error lb.lex_curr_p
  | Lexing_error p -> Lexing_error p

let parse_opamfile input =
  let filename =
    OpamFile.make (OpamFilename.of_string (Input.filename input))
  in
  match input with
  | Input.File { path } ->
      In_channel.with_open_bin path (OpamFile.OPAM.read_from_channel ~filename)
  | String { data } -> OpamFile.OPAM.read_from_string ~filename data

let parse_both ~debug_tokens ~debug_ast input =
  let r_exp = Input.with_lexbuf input ~f:(parse_lb ~debug_tokens ~debug_ast) in
  match r_exp with
  | (Lexing_error _ | Compile_error _ | Parse_error _) as r -> r
  | Different_result _ -> assert false
  | Success exp -> (
      let control = parse_opamfile input in
      match Compare.compare_opam_files exp control with
      | Ok () -> Success exp
      | Error message ->
          Different_result { filename = Input.filename input; message })

let parse_lb_off input =
  let filename = Input.filename input in
  let[@alert "-deprecated"] opamfile =
    match input with
    | Input.File { path } -> OpamParser.file path
    | String { data } -> OpamParser.string data filename
  in
  match Compile_off.compile opamfile with
  | Ok x -> Outcome.Success x
  | Error s -> Compile_error { filename; message = s }

let parse_both_off input =
  let r_exp = parse_lb_off input in
  let control = parse_opamfile input in
  match r_exp with
  | (Outcome.Lexing_error _ | Compile_error _ | Parse_error _) as r -> r
  | Outcome.Different_result _ -> assert false
  | Outcome.Success exp ->
      if OpamFile.OPAM.effectively_equal exp control then Success exp
      else
        Different_result { filename = Input.filename input; message = "differ" }

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

module Common = struct
  let debug_tokens = Cmdliner.Arg.(value & flag & info [ "debug-tokens" ])
  let debug_ast = Cmdliner.Arg.(value & flag & info [ "debug-ast" ])
  let repo_path = Cmdliner.Arg.(required & pos 0 (some string) None & info [])
end

let get_stats repo_path ~f =
  let stats = ref Stats.empty in
  iter_on_opam_files repo_path ~f:(fun path ->
      let input = Input.File { path } in
      let r = f input in
      stats := Stats.add !stats r);
  !stats

module Repo = struct
  let term =
    let open Let_syntax in
    let open Common in
    let+ repo_path
    and+ show_failure = Cmdliner.Arg.(value & flag & info [ "show" ])
    and+ stop = Cmdliner.Arg.(value & flag & info [ "stop" ])
    and+ debug_tokens
    and+ debug_ast
    and+ off = Cmdliner.Arg.(value & flag & info [ "off" ]) in
    let stats =
      get_stats repo_path ~f:(fun input ->
          let r =
            if off then parse_both_off input
            else parse_both ~debug_tokens ~debug_ast input
          in
          if show_failure then Outcome.report ~fatal:stop ~input r;
          r)
    in
    Format.printf "%a" Stats.pp stats

  let info = Cmdliner.Cmd.info "repo"
  let cmd = Cmdliner.Cmd.v info term
end

module Parse = struct
  let term =
    let open Let_syntax in
    let open Common in
    let+ debug_tokens and+ debug_ast in
    let data = In_channel.input_all In_channel.stdin in
    let data = "opam-version: \"2.0\"\n" ^ data in
    let input = Input.String { data } in
    parse_both ~debug_tokens ~debug_ast input
    |> Outcome.report ~input ~fatal:true

  let info = Cmdliner.Cmd.info "parse"
  let cmd = Cmdliner.Cmd.v info term
end

module Bench = struct
  type parser = Control | Experiment

  let term =
    let open Let_syntax in
    let open Common in
    let+ repo_path
    and+ parser =
      Cmdliner.Arg.(
        required
        & opt
            (some & enum [ ("control", Control); ("experiment", Experiment) ])
            None
        & info [ "parser" ])
    in
    let stats =
      get_stats repo_path ~f:(fun input ->
          match parser with
          | Control -> Success (parse_opamfile input)
          | Experiment ->
              Input.with_lexbuf input
                ~f:(parse_lb ~debug_tokens:false ~debug_ast:false))
    in
    Format.printf "%a" Stats.pp stats

  let info = Cmdliner.Cmd.info "bench"
  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "turbo-opam"
let cmd = Cmdliner.Cmd.group info [ Repo.cmd; Parse.cmd; Bench.cmd ]
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
