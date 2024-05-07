module Let_syntax = struct
  open Cmdliner.Term

  let ( let+ ) x f = const f $ x
end

let highlight (pos : Lexing.position) =
  let input = Pp_loc.Input.file pos.pos_fname in
  let ppf = Format.std_formatter in
  let location = Pp_loc.Position.of_lexing pos in
  let locs = [ (location, Pp_loc.Position.shift location 1) ] in
  Pp_loc.pp ~input ppf locs

type outcome = Lexing_error of Lexing.position | Ok

let rec iter_on_tokens lb ~f =
  match Lexer.token lb with
  | Some t ->
      f t;
      iter_on_tokens lb ~f
  | None -> Ok
  | exception Failure _ -> Lexing_error lb.lex_curr_p

let parse_exp path =
  In_channel.with_open_bin path (fun ic ->
      let lb = Lexing.from_channel ic in
      Lexing.set_filename lb path;
      iter_on_tokens lb ~f:(fun t ->
          if false then print_endline (Lexer.token_to_string t)))

let _report_outcome = function
  | Ok -> ()
  | Lexing_error pos ->
      Printf.printf "lexing error near:\n";
      highlight pos;
      exit 1

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
  type t = Ok | Lexing_error

  let pp ppf = function
    | Ok -> Format.fprintf ppf "Ok"
    | Lexing_error -> Format.fprintf ppf "Lexing error"

  let compare = Stdlib.compare

  let for_outcome : outcome -> t = function
    | Lexing_error _ -> Lexing_error
    | Ok -> Ok
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
  let+ repo_path =
    Cmdliner.Arg.(required & pos 0 (some string) None & info [])
  in
  let stats = ref Stats.empty in
  iter_on_opam_files repo_path ~f:(fun path ->
      (*let _control =*)
      (*let filename = OpamFile.make (OpamFilename.of_string path) in*)
      (*In_channel.with_open_bin path*)
      (*(OpamFile.OPAM.read_from_channel ~filename)*)
      (*in*)
      let r_exp = parse_exp path in
      stats := Stats.add !stats r_exp;
      if false then print_endline path);
  Format.printf "%a" Stats.pp !stats

let info = Cmdliner.Cmd.info "turbo-opam"
let cmd = Cmdliner.Cmd.v info term
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
