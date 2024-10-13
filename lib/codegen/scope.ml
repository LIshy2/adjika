open Llvm
open Core

module Resources = struct
  type obj = llvalue * Bind.BindType.t
  type t = obj list

  let concat = List.append
  let obj v t = (v, t)
  let single v t = [ obj v t ]
  let of_list l = l

  let empty = []
end



let desctruct resources runtime builder =
  List.iter resources ~f:(fun (v, t) ->
      match t with
      | Bind.BindType.Int ->
          let _ = Runtime.build_dedup runtime v "dedup" builder in
          ()
      | _ -> ())
