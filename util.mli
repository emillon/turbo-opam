val errorf : ('a, Format.formatter, unit, (_, string) result) format4 -> 'a
val map_m : f:('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
