open Util

let as_string ~context = function
  | Ast.V_string s -> Ok s
  | v -> errorf "in %s: not a string: %a" context Ast.pp_value v

let relop : Ast.op -> _ = function
  | Ge -> Ok `Geq
  | Le -> Ok `Leq
  | Lt -> Ok `Lt
  | Neq -> Ok `Neq
  | Eq -> Ok `Eq
  | Gt -> Ok `Gt

type 'a decoder = Ast.value -> ('a, string) result

let fident_of_string s =
  match String.split_on_char ':' s with
  | [ s ] -> Ok (OpamTypes.FIdent ([], OpamVariable.of_string s, None))
  | [ pre; s ] ->
      Ok
        (FIdent
           ( [ Some (OpamPackage.Name.of_string pre) ],
             OpamVariable.of_string s,
             None ))
  | l -> errorf "fident_of_string: %a" (Ast.pp_list Format.pp_print_string) l

let rec to_filter : OpamTypes.filter decoder =
  let open Result_let_syntax in
  function
  | V_group v -> to_filter v
  | V_list [ v ] -> to_filter v
  | V_string s -> Ok (FString s)
  | V_or (va, vb) ->
      let+ a = to_filter va and+ b = to_filter vb in
      OpamTypes.FOr (a, b)
  | V_and (va, vb) ->
      let+ a = to_filter va and+ b = to_filter vb in
      OpamTypes.FAnd (a, b)
  | V_op2 (va, op, vb) ->
      let+ a = to_filter va and+ relop = relop op and+ b = to_filter vb in
      OpamTypes.FOp (a, relop, b)
  | V_ident s -> fident_of_string s
  | V_not v ->
      let+ f = to_filter v in
      OpamTypes.FNot f
  | v -> errorf "to_filter: %a" Ast.pp_value v

let rec filter :
    OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula decoder
    =
  let open Result_let_syntax in
  let atom x = OpamFormula.Atom x in
  let atom_filter x = atom (OpamTypes.Filter x) in
  function
  | V_op (op, v) ->
      let+ relop = relop op and+ filter = to_filter v in
      atom (OpamTypes.Constraint (relop, filter))
  | V_and (va, vb) -> filters [ va; vb ]
  | V_or (va, vb) ->
      let+ a = filter va and+ b = filter vb in
      OpamFormula.Or (a, b)
  | V_ident s ->
      let+ fid = fident_of_string s in
      atom_filter fid
  | V_op2 (a, op, b) ->
      let+ fa = to_filter a and+ relop = relop op and+ fb = to_filter b in
      atom_filter (FOp (fa, relop, fb))
  | V_group v ->
      let+ f = filter v in
      OpamTypes.Block f
  | V_list l -> filters l
  | V_not v ->
      let+ f = to_filter v in
      atom_filter (FNot f)
  | v -> errorf "filter: %a" Ast.pp_value v

and filters :
    Ast.value list ->
    ( OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula,
      string )
    result =
 fun vs -> map_m ~f:filter vs |> Result.map OpamFormula.ands

let rec filtered_formula : OpamTypes.filtered_formula decoder =
  let open Result_let_syntax in
  function
  | V_group l ->
      let+ f = filtered_formula l in
      OpamFormula.Block f
  | V_list l -> map_m ~f:filtered_formula l |> Result.map OpamFormula.ands
  | V_filter (v, v_filters) ->
      let* name_s = as_string ~context:"filter" v in
      let name = OpamPackage.Name.of_string name_s in
      let+ filters = filters v_filters in
      OpamFormula.Atom (name, filters)
  | V_string v ->
      let name = OpamPackage.Name.of_string v in
      Ok (OpamFormula.Atom (name, OpamFormula.Empty))
  | V_or (a, b) ->
      let* fa = filtered_formula a in
      let+ fb = filtered_formula b in
      OpamFormula.Or (fa, fb)
  | V_and (a, b) ->
      let* fa = filtered_formula a in
      let+ fb = filtered_formula b in
      OpamFormula.And (fa, fb)
  | v -> errorf "filtered_formula: %a" Ast.pp_value v

let add_filter arg f =
  match arg with
  | _, Some _ -> errorf "add_filter: several filters"
  | sa, None -> Ok (sa, Some f)

let rec arg : OpamTypes.arg decoder =
  let open Result_let_syntax in
  function
  | V_ident s -> Ok (CIdent s, None)
  | V_string s -> Ok (CString s, None)
  | V_filter (v, [ vf ]) ->
      let* arg = arg v in
      let* f = to_filter vf in
      add_filter arg f
  | v -> errorf "arg: %a" Ast.pp_value v

let args : OpamTypes.arg list decoder =
  let open Result_let_syntax in
  function
  | (V_string _ | V_ident _ | V_filter _) as v ->
      let+ arg = arg v in
      [ arg ]
  | V_list l -> map_m ~f:arg l
  | v -> errorf "args: %a" Ast.pp_value v

let command : OpamTypes.command decoder =
  let open Result_let_syntax in
  function
  | V_filter ((V_list _ as v), [ vf ]) ->
      let+ args = args v and+ filter = to_filter vf in
      (args, Some filter)
  | V_filter (V_string _, _) as v ->
      let+ args = args v in
      (args, None)
  | (V_list _ | V_string _ | V_ident _) as v ->
      let+ args = args v in
      (args, None)
  | v -> errorf "command: %a" Ast.pp_value v

let commands : OpamTypes.command list decoder =
  let open Result_let_syntax in
  function
  | V_list [] -> Ok [ ([], None) ]
  | ( V_string _ | V_ident _ | V_filter _
    | V_list ((V_string _ | V_ident _) :: _) ) as v ->
      let+ c = command v in
      [ c ]
  | V_list l -> map_m ~f:command l
  | v -> errorf "commands: %a" Ast.pp_value v

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
      | [ [ "build" ] ] ->
          let+ cmds = commands v in
          OpamFile.OPAM.with_build cmds opam
      | [ [ "depends" ] ] ->
          let+ depends = filtered_formula v in
          OpamFile.OPAM.with_depends depends opam
      | [ [ "depopts" ] ] ->
          let+ depopts = filtered_formula v in
          OpamFile.OPAM.with_depopts depopts opam
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
