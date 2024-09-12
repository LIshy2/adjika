type 'a t

val ( *> ) : ('a -> 'b) t -> ('b, 'c, 'd) Phase.t -> ('a -> 'c) t
val pipeline : ('a, 'b, 'c) Phase.t -> ('a -> 'b) t
val phase_execute : ('a, 'b, 'c) Phase.t -> 'a -> 'b
val execute : 'inp -> ('inp -> 'outp) t -> 'outp
