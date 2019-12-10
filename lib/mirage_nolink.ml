open Resolver

type ident =
  { name : Caml_name.t
  ; kind : Caml_obj.kind
  ; path : Fpath.t }

type resolver =
  ident -> Digest.t option -> artifact list -> artifact list * artifact list

let resolvers : (Caml_name.t, resolver) Hashtbl.t = Hashtbl.create 0x100

let to_ident x =
  { name= Caml_obj.name x
  ; kind= Caml_obj.kind x
  ; path= Caml_obj.path x }

let register ~name resolve =
  Hashtbl.add resolvers name resolve

let select ~kind who dep res =
  let res' = List.filter (function
      | `Caml x -> Caml_obj.kind x = kind
      | `Object _ -> kind = Caml_obj.CMX)
      res in
  match res' with
  | [ x ] -> [ x ]
  | res' ->
    let res = match res' with [] -> res | res -> res in
    let name, digest = dep in
    match Hashtbl.find resolvers name with
    | resolver ->
      ( match resolver who digest res with
        | [], _ ->
          Fmt.epr "[%a] No solution found to: %a.\n%!"
            Fmt.(styled `Red string) "ERR"
            Caml_name.pp name ;
          []
        | [ x ], others -> x :: others (* XXX(dinosaure): we want the totality! *)
        | _some, _others -> assert false )
    | exception Not_found ->
      Fmt.pr ">>> Need a solution for [%a].\n%!" Caml_name.pp name ;
      let pp = function
        | `Object x -> Fmt.pr "> %a\n%!" Obj.pp x
        | `Caml x -> Fmt.pr "> %a\n%!" Caml_obj.pp x in
      List.iter pp res ;
      assert false (* TODO: REPL! *)

let take ~kind ?(archive= true) ~name res =
  let filter = function
    | `Object _ -> archive && kind = Caml_obj.CMX
    | `Caml x -> match kind, archive, name with
      | kind, true, name ->
        Caml_name.equal name (Caml_obj.name x)
        && Caml_obj.kind x = kind
        && Caml_obj.in_archive x
      | kind, false, name ->
        Caml_name.equal name (Caml_obj.name x)
        && Caml_obj.kind x = kind
        && not (Caml_obj.in_archive x) in
  List.filter filter res

module Base = struct
  type 'a t = 'a

  let return x = x
  let map x ~f = f x
  let apply f x = f x
  let select e f = match e with
    | Selective.Either.L x -> f x
    | Selective.Either.R y -> y
end

module Selective = Selective.Make(Base)
module Gamma = Gamma(struct include Selective let run x = x end)
module Resolver = Make(Gamma)

type error = ident Resolver.error

let pp_error ppf = function
  | `Unresolved_dependency (ident, dep) ->
    let name, _ = dep in
    Fmt.pf ppf "Unresolved dependency %a requested by %a"
      Caml_name.pp name
      Fpath.pp ident.path
  | `Msg err -> Fmt.string ppf err

let resolve ~kind ~srcs gamma =
  Resolver.resolve to_ident ~select:(select ~kind) ~srcs gamma
