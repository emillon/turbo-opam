open Util

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
  else if a.depends <> b.depends then
    errorf "depends differs:\n%a\n%a" Pp.filtered_formula a.depends
      Pp.filtered_formula b.depends
  else if a.depopts <> b.depopts then
    errorf "depopts differs:\n%a\n%a" Pp.filtered_formula a.depopts
      Pp.filtered_formula b.depopts
  else if a.build <> b.build then
    errorf "build differs:\n%a\n%a" Pp.commands a.build Pp.commands b.build
  else if a.conflicts <> b.conflicts then
    errorf "conflicts differs:\n%a\n%a" Pp.filtered_formula a.conflicts
      Pp.filtered_formula b.conflicts
  else if a.available <> b.available then
    errorf "available differs:\n%a\n%a" Pp.filter a.available Pp.filter
      b.available
  else if a.conflict_class <> b.conflict_class then
    errorf "conflict-class differs:\n%a\n%a" (Pp.list Pp.name) a.conflict_class
      (Pp.list Pp.name) b.conflict_class
  else if a.install <> b.install then
    errorf "install differs:\n%a\n%a" Pp.commands a.install Pp.commands
      b.install
  else Ok ()
(*
     OpamFile.OPAM.effective_part

   {

     env        = t.env;

     run_test   = t.deprecated_build_test @ t.run_test;
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
