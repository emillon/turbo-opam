let filtered_formula ppf f =
  Format.pp_print_string ppf (OpamFilter.string_of_filtered_formula f)

let list pp ppf l =
  let first = ref true in
  Format.fprintf ppf "[";
  List.iter
    (fun x ->
      if !first then first := false else Format.fprintf ppf "; ";
      pp ppf x)
    l;
  Format.fprintf ppf "]"

let option pp ppf = function
  | None -> Format.fprintf ppf "None"
  | Some x -> Format.fprintf ppf "Some (%a)" pp x

let name ppf x = Format.fprintf ppf "%s" (OpamPackage.Name.to_string x)
let var ppf var = Format.fprintf ppf "%s" (OpamVariable.to_string var)
let pair pp_a pp_b ppf (a, b) = Format.fprintf ppf "(%a, %a)" pp_a a pp_b b

let ident ppf (name_opt_list, v, string_string_opt) =
  Format.fprintf ppf "(%a, %a, %a)"
    (list (option name))
    name_opt_list var v
    (option (pair Format.pp_print_string Format.pp_print_string))
    string_string_opt

let relop ppf op = Format.fprintf ppf "%s" (OpamPrinter.FullPos.relop_kind op)

let rec filter ppf = function
  | OpamTypes.FBool _ -> Format.fprintf ppf "FBool _"
  | FString _ -> Format.fprintf ppf "FString _"
  | FIdent i -> Format.fprintf ppf "FIdent %a" ident i
  | FOp (a, op, b) ->
      Format.fprintf ppf "FOp (%a, %a, %a)" filter a relop op filter b
  | FAnd (a, b) -> Format.fprintf ppf "FAnd (%a, %a)" filter a filter b
  | FOr _ -> Format.fprintf ppf "FOr _"
  | FNot _ -> Format.fprintf ppf "FNot _"
  | FDefined _ -> Format.fprintf ppf "FDefined _"
  | FUndef _ -> Format.fprintf ppf "FUndef _"

let filter_opt ppf = function
  | None -> ()
  | Some f -> Format.fprintf ppf " { %a }" filter f

let simple_arg ppf = function
  | OpamTypes.CString s -> Format.fprintf ppf "%S" s
  | OpamTypes.CIdent s -> Format.fprintf ppf "%s" s

let arg ppf (sa, filter_o) =
  Format.fprintf ppf "%a%a" simple_arg sa filter_opt filter_o

let command ppf (args, filter_o) =
  Format.fprintf ppf "%a%a" (list arg) args filter_opt filter_o

let commands = list command
