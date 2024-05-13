type op = Ge | Lt | Le | Eq | Gt
type op2 = Eq2 | Neq2 | Ge2

type value =
  | V_string of string
  | V_list of value list
  | V_ident of string
  | V_var of string * string
  | V_filter of value * filter
  | V_filter_value of filter
  | V_or of value * value

and filter =
  | F_op of op * value
  | F_op2 of value * op2 * value
  | F_and of filter * filter
  | F_or of filter * filter
  | F_ident of string

type t = { sections : (string list list * value) list; filename : string }
