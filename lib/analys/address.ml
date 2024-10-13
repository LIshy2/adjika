open Core
open Parsing
open Typing
open Capture
open Core.List.Let_syntax

module Interactor = struct
  module Id = struct
    module T = struct
      type t = Id of int [@@deriving sexp, compare]
    end

    include T
    include Comparator.Make (T)
  end

  let id num = Id.Id num

  type 'e statement =
    | Spawn of { name : string; actor : 'e }
    | Val of Cexp.t Texp.TypedVal.t
    | Mutate of 'e
    | Send of { message : 'e; mail : 'e; destination : Id.t }

  type ('e, 't) handler = {
    message_type : 't;
    state : string;
    body : 'e statement list;
  }

  type ('e, 't) t = {
    actor : string;
    id : Id.t;
    handlers : ('e, 't) handler list;
  }

  let handler message_type state body = { message_type; state; body }
  let decl actor id handlers = { actor; id; handlers }
  let message_type interactor = (List.hd_exn interactor.handlers).message_type
end

module AProgram = struct
  type t = {
    functions : Cexp.t Texp.TProgram.tfun list;
    types : (Type.mono, int) Ast.Datatype.t list;
    actors : Type.mono Ast.Actor.t list;
    interactors : (Cexp.t, Type.mono) Interactor.t list;
  }
end

module InteractorTag = struct
  module T = struct
    type t = Id of (string * Type.mono) [@@deriving compare, sexp]

    let actor (Id (name, _)) = name
  end

  include T
  include Comparator.Make (T)
end

let handler_direction interactor_map handler =
  let open Ast.Handler in
  let body =
    List.map handler.body ~f:(fun statement ->
        match statement with
        | Spawn { name; actor } -> Interactor.Spawn { name; actor }
        | Val def -> Interactor.Val def
        | Mutate b -> Interactor.Mutate b
        | Send { message; mail } ->
            let actor_receiver =
              match Cexp.type_of mail with
              | Type.MailBox name -> name
              | _ -> failwith "a"
            in
            let message_type = Cexp.type_of message in
            let destination =
              Map.find_exn interactor_map
                (InteractorTag.Id (actor_receiver, message_type))
            in
            Interactor.Send { message; mail; destination })
  in
  Interactor.handler handler.message_type handler.state body

let connect program =
  let open Texp.TProgram in
  let state_to_actor =
    let map =
      Map.of_alist_exn
        (module String)
        (let%bind actor = program.actors in
         let%map state = actor.states in
         (state.name, actor.name))
    in
    Map.find_exn map
  in
  let tags =
    List.map program.handlers ~f:(fun handler ->
        let actor = state_to_actor handler.state in
        InteractorTag.Id (actor, handler.message_type))
  in
  let interactor_tags =
    let dedup = List.dedup_and_sort tags ~compare:InteractorTag.compare in
    let indexed = List.mapi dedup ~f:(fun ind d -> (d, Interactor.Id.Id ind)) in
    Map.of_alist_exn (module InteractorTag) indexed
  in

  let handler_groups =
    let tagged_handlers =
      List.map program.handlers ~f:(fun handler ->
          let actor = state_to_actor handler.state in
          (InteractorTag.Id (actor, handler.message_type), handler))
    in
    Map.to_alist (Map.of_alist_multi (module InteractorTag) tagged_handlers)
  in
  let interactors =
    List.map handler_groups ~f:(fun (id, handlers) ->
        let tag = Map.find_exn interactor_tags id in
        let actor = InteractorTag.actor id in
        let handlers =
          List.map handlers ~f:(handler_direction interactor_tags)
        in
        Interactor.decl actor tag handlers)
  in
  AProgram.
    {
      functions = program.functions;
      types = program.types;
      actors = program.actors;
      interactors;
    }
