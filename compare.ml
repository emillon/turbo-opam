open Util

let pp_simple_arg ppf = function
  | OpamTypes.CString s -> Format.fprintf ppf "%S" s
  | OpamTypes.CIdent s -> Format.fprintf ppf "%s" s

let pp_filter_opt ppf = function
  | None -> ()
  | Some _ -> Format.fprintf ppf " { _ }"

let pp_arg ppf (sa, filter_o) =
  Format.fprintf ppf "%a%a" pp_simple_arg sa pp_filter_opt filter_o

let pp_command ppf (args, filter_o) =
  Format.fprintf ppf "%a%a" (Ast.pp_list pp_arg) args pp_filter_opt filter_o

let pp_commands = Ast.pp_list pp_command

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
  let pp_filtered_formula ppf f =
    Format.pp_print_string ppf (OpamFilter.string_of_filtered_formula f)
  in
  if a.name <> b.name then
    errorf "name differs: %a %a" pp_name_opt a.name pp_name_opt b.name
  else if a.version <> b.version then
    errorf "version differs: %a %a" pp_version_opt a.version pp_version_opt
      b.version
  else if a.depends <> b.depends then
    errorf "depends differs:\n%a\n%a" pp_filtered_formula a.depends
      pp_filtered_formula b.depends
  else if a.build <> b.build then
    errorf "build differs:\n%a\n%a" pp_commands a.build pp_commands b.build
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
