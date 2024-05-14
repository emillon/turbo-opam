val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
