type 'a t := Format.formatter -> 'a -> unit

val list : 'a t -> 'a list t
val filtered_formula : OpamTypes.filtered_formula t
val commands : OpamTypes.command list t
val filter : OpamTypes.filter t
