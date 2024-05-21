type op = Ge | Lt | Le | Eq | Gt | Neq

let pp_op ppf = function
  | Ge -> Format.fprintf ppf "Ge"
  | Lt -> Format.fprintf ppf "Lt"
  | Le -> Format.fprintf ppf "Le"
  | Eq -> Format.fprintf ppf "Eq"
  | Gt -> Format.fprintf ppf "Gt"
  | Neq -> Format.fprintf ppf "Neq"

type env_op = PlusEq

type value =
  | V_string of string
  | V_list of value list
  | V_ident of string
  | V_var of string * string
  | V_filter of value * value list
  | V_or of value * value
  | V_op of op * value
  | V_op2 of value * op * value
  | V_envop of value * env_op * value
  | V_and of value * value
  | V_not of value
  | V_group of value

let rec pp_value ppf = function
  | V_string s -> Format.fprintf ppf "V_string %S" s
  | V_list v -> Format.fprintf ppf "V_list %a" (Pp.list pp_value) v
  | V_ident s -> Format.fprintf ppf "V_ident %S" s
  | V_var _ -> Format.fprintf ppf "V_var _"
  | V_filter (v, fs) ->
      Format.fprintf ppf "V_filter (%a, %a)" pp_value v (Pp.list pp_value) fs
  | V_or (a, b) -> Format.fprintf ppf "V_or (%a, %a)" pp_value a pp_value b
  | V_op (op, v) -> Format.fprintf ppf "V_op (%a, %a)" pp_op op pp_value v
  | V_op2 (a, op, b) ->
      Format.fprintf ppf "V_op2 (%a, %a, %a)" pp_value a pp_op op pp_value b
  | V_envop _ -> Format.fprintf ppf "V_envop _"
  | V_and (a, b) -> Format.fprintf ppf "V_and (%a, %a)" pp_value a pp_value b
  | V_not v -> Format.fprintf ppf "V_not (%a)" pp_value v
  | V_group v -> Format.fprintf ppf "V_group (%a)" pp_value v

type t = { sections : (string list list * value) list; filename : string }

let pp ppf { sections; filename = _ } =
  List.iter
    (fun (k, v) ->
      Format.fprintf ppf "%a\n%a\n"
        (Pp.list (Pp.list Format.pp_print_string))
        k pp_value v)
    sections
