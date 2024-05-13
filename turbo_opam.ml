module Let_syntax = struct
  open Cmdliner.Term

  let pair x y = (x, y)
  let ( let+ ) x f = const f $ x
  let ( and+ ) tx ty = const pair $ tx $ ty
end

let highlight ?input (pos : Lexing.position) =
  let input =
    match input with
    | Some s -> Pp_loc.Input.string s
    | None -> Pp_loc.Input.file pos.pos_fname
  in
  let ppf = Format.std_formatter in
  let location = Pp_loc.Position.of_lexing pos in
  let locs = [ (location, Pp_loc.Position.shift location 1) ] in
  Pp_loc.pp ~input ppf locs

type outcome =
  | Lexing_error of Lexing.position
  | Parse_error of Lexing.position
  | Compile_error of { filename : string; message : string }
  | Different_result of { message : string }
  | Success of OpamFile.OPAM.t

let report_outcome ?input = function
  | Success _ -> ()
  | Different_result { message } ->
      Printf.printf "different result: %s\n" message;
      exit 1
  | Lexing_error pos ->
      Printf.printf "lexing error near:\n";
      highlight ?input pos;
      exit 1
  | Parse_error pos ->
      Printf.printf "parse error in %s near:\n" pos.pos_fname;
      highlight ?input pos;
      exit 1
  | Compile_error { filename; message } ->
      Printf.printf "compile error in %s: %s\n" filename message;
      exit 1

let errorf fmt = Format.kasprintf Result.error fmt

let as_string ~context = function
  | Ast.V_string s -> Ok s
  | _ -> errorf "in %s: not a string" context

module Result_let_syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) x f = Result.map f x
end

let compile { Ast.sections; filename } =
  let pkg =
    OpamFilename.of_string filename |> OpamPackage.of_filename |> Option.get
  in
  List.fold_left
    (fun acc (k, v) ->
      let open Result_let_syntax in
      let* opam = acc in
      match k with
      | [ [ ("opam-version" as context) ] ] -> (
          let* s = as_string ~context v in
          match s with "2.0" -> Ok opam | _ -> Error "unknown opam-version")
      | [ [ "maintainer" ] ] -> (* TODO set it *) Ok opam
      | [ [ "authors" ] ] -> (* TODO set it *) Ok opam
      | [ [ "homepage" ] ] -> (* TODO set it *) Ok opam
      | [ [ "bug-reports" ] ] -> (* TODO set it *) Ok opam
      | [ [ ("dev-repo" as context) ] ] ->
          let+ s = as_string ~context v in
          let url = OpamUrl.of_string s in
          OpamFile.OPAM.with_dev_repo url opam
      | [ [ "build" ] ] -> (* TODO set it *) Ok opam
      | [ [ "depends" ] ] -> (* TODO set it *) Ok opam
      | [ [ "depopts" ] ] -> (* TODO set it *) Ok opam
      | [ [ "depexts" ] ] -> (* TODO set it *) Ok opam
      | [ [ "synopsis" ] ] -> (* TODO set it *) Ok opam
      | [ [ "description" ] ] -> (* TODO set it *) Ok opam
      | [ [ "extra-files" ] ] -> (* TODO set it *) Ok opam
      | [ [ "conflicts" ] ] -> (* TODO set it *) Ok opam
      | [ [ "doc" ] ] -> (* TODO set it *) Ok opam
      | [ [ "license" ] ] -> (* TODO set it *) Ok opam
      | [ [ "x-commit-hash" ] ] -> (* TODO set it *) Ok opam
      | [ [ "x-opam-monorepo-opam-provided" ] ] -> (* TODO set it *) Ok opam
      | [ [ "x-ci-accept-failures" ] ] -> (* TODO set it *) Ok opam
      | [ [ "patches" ] ] -> (* TODO set it *) Ok opam
      | [ [ "post-messages" ] ] -> (* TODO set it *) Ok opam
      | [ [ "messages" ] ] -> (* TODO set it *) Ok opam
      | [ [ "tags" ] ] -> (* TODO set it *) Ok opam
      | [ [ "install" ] ] -> (* TODO set it *) Ok opam
      | [ [ "remove" ] ] -> (* TODO set it *) Ok opam
      | [ [ "flags" ] ] -> (* TODO set it *) Ok opam
      | [ [ "substs" ] ] -> (* TODO set it *) Ok opam
      | [ [ "available" ] ] -> (* TODO set it *) Ok opam
      | [ [ "run-test" ] ] -> (* TODO set it *) Ok opam
      | [ [ "version" ] ] -> (* TODO set it *) Ok opam
      | [ [ "author" ] ] -> (* TODO set it *) Ok opam
      | [ [ "build-env" ] ] -> (* TODO set it *) Ok opam
      | [ [ "conflict-class" ] ] -> (* TODO set it *) Ok opam
      | [ [ "pin-depends" ] ] -> (* TODO set it *) Ok opam
      | [ [ "setenv" ] ] -> (* TODO set it *) Ok opam
      | [ [ "name" ] ] -> (* TODO set it *) Ok opam
      | [ [ "url" ]; [ "src" ] ] -> (* TODO set it *) Ok opam
      | [ [ "url" ]; [ "mirrors" ] ] -> (* TODO set it *) Ok opam
      | [ [ "url" ]; [ "checksum" ] ] -> (* TODO set it *) Ok opam
      | [ [ "url" ]; [ "archive" ] ] -> (* TODO set it *) Ok opam
      | [ [ "url" ]; [ "git" ] ] -> (* TODO set it *) Ok opam
      | [ [ "extra-source"; _ ]; [ "src" ] ] -> (* TODO set it *) Ok opam
      | [ [ "extra-source"; _ ]; [ "checksum" ] ] -> (* TODO set it *) Ok opam
      | _ -> errorf "unknown key: %s" (String.concat "." (List.concat k)))
    (Ok (OpamFile.OPAM.create pkg))
    sections

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

let parse_lb ~debug_tokens lb =
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
    match compile ast with
    | Ok r -> Success r
    | Error message -> Compile_error { filename = ast.filename; message }
  with
  | Parser.Error -> Parse_error lb.lex_curr_p
  | Lexing_error p -> Lexing_error p

let parse_exp path =
  In_channel.with_open_bin path (fun ic ->
      let lb = Lexing.from_channel ic in
      Lexing.set_filename lb path;
      parse_lb ~debug_tokens:false lb)

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

  let for_outcome : outcome -> t = function
    | Lexing_error _ -> Lexing_error
    | Success _ -> Success
    | Parse_error _ -> Parse_error
    | Compile_error _ -> Compile_error
    | Different_result _ -> Different_result
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

let compare_opam_files (a : OpamFile.OPAM.t) (b : OpamFile.OPAM.t) =
  let pp_name_opt ppf o =
    let s =
      match o with None -> "<none>" | Some p -> OpamPackage.Name.to_string p
    in
    Format.pp_print_string ppf s
  in
  let pp_version_opt ppf o =
    let s =
      match o with
      | None -> "<none>"
      | Some p -> OpamPackage.Version.to_string p
    in
    Format.pp_print_string ppf s
  in
  if a.name <> b.name then
    errorf "name differs: %a %a" pp_name_opt a.name pp_name_opt b.name
  else if a.version <> b.version then
    errorf "version differs: %a %a" pp_version_opt a.version pp_version_opt
      b.version
  else Ok ()
(*
     OpamFile.OPAM.effective_part

   {

     depends    = t.depends;
     depopts    = t.depopts;
     conflicts  = t.conflicts;
     conflict_class = t.conflict_class;
     available  = t.available;
     flags      =
       (List.filter (function
            | Pkgflag_LightUninstall
            | Pkgflag_Verbose
            | Pkgflag_Plugin
            | Pkgflag_Compiler
            | Pkgflag_Conf
            | Pkgflag_AvoidVersion
            | Pkgflag_Unknown _
              -> false)
           t.flags);
     env        = t.env;

     build      = t.build;
     run_test   = t.deprecated_build_test @ t.run_test;
     install    = t.install;
     remove     = t.remove;

     substs     = t.substs;
     patches    = t.patches;
     build_env  = t.build_env;
     features   = t.features;
     extra_sources = t.extra_sources;

     url         =
       (match t.url with
        | None -> None
        | Some u -> match URL.checksum u with
          | [] -> Some (URL.create (URL.url u)) (* ignore mirrors *)
          | cksum::_ ->
            Some (URL.with_checksum [cksum] URL.empty));
            (* ignore actual url and extra checksums *)

     extra_files = OpamStd.Option.Op.(t.extra_files ++ Some []);

     deprecated_build_doc = t.deprecated_build_doc;
   }
*)

module Repo = struct
  let term =
    let open Let_syntax in
    let+ repo_path =
      Cmdliner.Arg.(required & pos 0 (some string) None & info [])
    and+ fail = Cmdliner.Arg.(value & flag & info [ "fail" ]) in
    let stats = ref Stats.empty in
    iter_on_opam_files repo_path ~f:(fun path ->
        let r_exp = parse_exp path in
        let r =
          match r_exp with
          | (Lexing_error _ | Compile_error _ | Parse_error _) as r -> r
          | Different_result _ -> assert false
          | Success exp -> (
              let control =
                let filename = OpamFile.make (OpamFilename.of_string path) in
                In_channel.with_open_bin path
                  (OpamFile.OPAM.read_from_channel ~filename)
              in
              match compare_opam_files exp control with
              | Ok () -> Success exp
              | Error message -> Different_result { message })
        in
        if fail then report_outcome r;
        stats := Stats.add !stats r;
        if false then print_endline path);
    Format.printf "%a" Stats.pp !stats

  let info = Cmdliner.Cmd.info "repo"
  let cmd = Cmdliner.Cmd.v info term
end

module Parse = struct
  let term =
    let open Let_syntax in
    let+ debug_tokens = Cmdliner.Arg.(value & flag & info [ "debug-tokens" ]) in
    let input = In_channel.input_all In_channel.stdin in
    let lb = Lexing.from_string input in
    parse_lb ~debug_tokens lb |> report_outcome ~input

  let info = Cmdliner.Cmd.info "parse"
  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "turbo-opam"
let cmd = Cmdliner.Cmd.group info [ Repo.cmd; Parse.cmd ]
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
