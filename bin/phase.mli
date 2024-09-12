type ('inp, 'outp, 'e) t =
  | WithError of ('inp -> ('outp, 'e) result) * ('e -> unit)
  | Pure of ('inp -> 'outp)

val pure : ('a -> 'b) -> ('a, 'b, 'c) t
val with_error : ('a -> ('b, 'c) result) -> ('c -> unit) -> ('a, 'b, 'c) t
