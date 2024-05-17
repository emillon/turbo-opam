open Util

let as_string ~context = function
  | Ast.V_string s -> Ok s
  | v -> errorf "in %s: not a string: %a" context Ast.pp_value v

let relop : Ast.op -> _ = function
  | Ge -> Ok `Geq
  | Le -> Ok `Leq
  | Lt -> Ok `Lt
  | Neq -> Ok `Neq
  | op -> errorf "relop: %a" Ast.pp_op op

type 'a decoder = Ast.value -> ('a, string) result

let rec to_filter : OpamTypes.filter decoder =
  let open Result_let_syntax in
  function
  (*| _ when true -> Ok (FBool false)*)
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
  | V_ident s -> Ok (OpamTypes.FIdent ([], OpamVariable.of_string s, None))
  | v -> errorf "to_filter: %a" Ast.pp_value v

let rec filter :
    OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula decoder
    =
  let open Result_let_syntax in
  function
  | V_op (op, v) ->
      let+ relop = relop op and+ filter = to_filter v in
      OpamFormula.Atom (OpamTypes.Constraint (relop, filter))
  | V_and (va, vb) -> filters [ va; vb ]
  | V_or (va, vb) ->
      let+ a = filter va and+ b = filter vb in
      OpamFormula.Or (a, b)
  | V_ident s ->
      Ok
        (OpamFormula.Atom
           (OpamTypes.Filter
              (OpamTypes.FIdent ([], OpamVariable.of_string s, None))))
  | v -> errorf "filter: %a" Ast.pp_value v

and filters :
    Ast.value list ->
    ( OpamTypes.filter OpamTypes.filter_or_constraint OpamFormula.formula,
      string )
    result =
 fun vs -> List.map filter vs |> traverse |> Result.map OpamFormula.ands

let rec filtered_formula : OpamTypes.filtered_formula decoder =
  let open Result_let_syntax in
  function
  | V_list l ->
      List.map filtered_formula l |> traverse |> Result.map OpamFormula.ands
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
  | v -> errorf "filtered_formula: %a" Ast.pp_value v

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
      | [ [ "depends" ] ] ->
          let+ depends = filtered_formula v in
          OpamFile.OPAM.with_depends depends opam
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
