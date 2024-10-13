open Core
open Llvm

module BindType = struct
  type t =
    | Int
    | Float
    | Bool
    | Ptr
    | Arrow of t list * t
    | Named of string
    | Operator of string * t list
    | MailBox of string
    | Actor of string
  [@@deriving compare, sexp]
end

type t = Single of llvalue | Versions of (BindType.t * llvalue) list
type allocation = Local | Heap

exception PolyBindError

let single_exn = function Single v -> v | _ -> raise PolyBindError
let versions_exn = function Versions v -> v | _ -> raise PolyBindError

exception UnfoundRealization

let realization bind bind_type =
  match bind with
  | Single mono_val -> mono_val
  | Versions specializations -> (
      match
        List.find specializations ~f:(fun (fun_tpe, _) ->
            BindType.compare fun_tpe bind_type = 0)
      with
      | Some (_, found) -> found
      | None -> raise UnfoundRealization)

let set_name bind name =
  match bind with
  | Single s ->
      set_value_name name s;
      ()
  | Versions v ->
      List.iter v ~f:(fun (_, v) -> set_value_name name v);
      ()
