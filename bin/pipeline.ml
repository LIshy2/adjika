type 'a t =
  | Cons : ('inp, 'outp, 'e) Phase.t * ('ot -> 'inp) t -> ('ot -> 'outp) t
  | Pipeline : ('inp, 'outp, 'e) Phase.t -> ('inp -> 'outp) t

let ( *> ) tail head = Cons (head, tail)
let pipeline b = Pipeline b

let phase_execute phase input =
  match phase with
  | Phase.WithError (f, handler) -> (
      match f input with
      | Result.Ok v -> v
      | Result.Error e ->
          let _ = handler e in
          exit (-1))
  | Phase.Pure f -> f input

let rec execute : type inp outp. inp -> (inp -> outp) t -> outp =
 fun input pipeline ->
  match pipeline with
  | Cons (current, other) -> phase_execute current (execute input other)
  | Pipeline first -> phase_execute first input
