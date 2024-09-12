type ('inp, 'outp, 'e) t = 
| WithError of ('inp -> ('outp, 'e) result) * ('e -> unit)
| Pure of ('inp -> 'outp)


let pure f = Pure f
let with_error f handler = 
  WithError (f, handler)