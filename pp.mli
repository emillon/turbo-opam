type 'a t := Format.formatter -> 'a -> unit

val option : 'a t -> 'a option t
val list : 'a t -> 'a list t
val filtered_formula : OpamTypes.filtered_formula t
val commands : OpamTypes.command list t
val filter : OpamTypes.filter t
val name : OpamPackage.Name.t t
val patch : (OpamFilename.Base.t * OpamTypes.filter option) t
val extra_file : (OpamFilename.Base.t * OpamHash.t) t
val basename : OpamFilename.Base.t t
