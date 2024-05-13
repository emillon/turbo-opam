type op = Ge | Lt | Le | Eq | Gt | Neq
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

type t = { sections : (string list list * value) list; filename : string }
