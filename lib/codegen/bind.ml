open Core
open Llvm

module BindType = struct
  type t =
    | Int
    | Ptr
    | Arrow of t list * t
    | Named of string
    | Operator of string * t list
    | MailBox of string
    | Actor of string
  [@@deriving compare, sexp]
end

type t = Single of llvalue | Versions of (BindType.t * llvalue) list

exception PolyBindError

let single_exn = function Single v -> v | _ -> raise PolyBindError
let versions_exn = function Versions v -> v | _ -> raise PolyBindError

let set_name bind name =
  match bind with
  | Single s ->
      set_value_name name s;
      ()
  | Versions v ->
      List.iter v ~f:(fun (_, v) -> set_value_name name v);
      ()
