open Rresult

module Digest = struct
  include Digest

  let pp = Fmt.using Digest.to_hex Fmt.string
end

let recognize_argument v =
  Bos.OS.Path.exists v >>= function
  | true when Fpath.is_dir_path v -> R.ok (`Lookup v)
  | false -> R.error_msgf "<%a> does not exists" Fpath.pp v
  | true -> match Fpath.get_ext v with
    | ".cmi" | ".cmx" | ".cmo" | ".cmt" | ".cma" | ".cmti" | ".cmxa" ->
      Resolver.Artifact.of_path v >>| fun v -> `Root v
    | ext -> R.error_msgf "Invalid extension of <%a> (%s)" Fpath.pp v ext

module Base = struct
  type 'a t = 'a

  let return x = x
  let apply f x = f x
  let map x ~f = f x
  let select e f = match e with
    | Selective.Either.L x -> f x
    | Selective.Either.R y -> y
end

module Selective = Selective.Make(Base)
module Gamma = Resolver.Gamma(struct include Selective let run x = x end)

let populate roots =
  let rec select pending acc = function
    | [] -> go pending acc
    | hd :: tl ->
      Bos.OS.Path.stat hd >>= fun stat ->

      match stat.Unix.st_kind with
      | Unix.S_DIR -> select (Fpath.to_dir_path hd :: pending) acc tl
      | Unix.S_REG ->
        ( match Fpath.get_ext hd with
          | ".cmx" | ".cmo" | ".cmi" | ".cmt" | ".cma" | ".cmti" | ".cmxa" ->
            ( match Resolver.Artifact.of_path hd with
              | Ok vs -> select pending (Gamma.populate vs acc) tl
              | Error err ->
                Fmt.epr "[%a] %a.\n%!" Fmt.(styled `Yellow string) "WARN" Caml_objinfo.pp_error err ;
                select pending acc tl )
          | _ -> select pending acc tl )
      | _ -> select pending acc tl
  and go pending acc = match pending with
    | [] -> Ok acc
    | x :: r ->
      Bos.OS.Dir.contents ~dotfiles:false ~rel:false x >>= select r acc in
  go roots Gamma.empty

let select (name, kind, _) dep res =
  let res = List.filter (function
      | `Caml x -> Resolver.Artifact.kind x = kind
      | `Object _ -> kind = Resolver.Artifact.CMX) res in
  match res with
  | [] ->
    let dep, _ = dep in
    Fmt.pr "[%s] wants [%s] but no caml objects exist.\n%!" name dep ;
    let rec repl () =
      Fmt.pr ">>> %!" ;
      match input_line stdin with
      | path -> match Fpath.of_string path with
        | Error _ -> Fmt.epr "<<< Invalid path.\n%!" ; repl ()
        | Ok path -> Bos.OS.File.exists path |> function
          | Ok false -> Fmt.epr "<<< %a does not exists.\n%!" Fpath.pp path ; repl ()
          | Error (`Msg err)  -> Fmt.epr "<<< %s.\n%!" err ; repl ()
          | Ok true -> match Fpath.get_ext path with
            | ".a" -> [ `Object path ]
            | _ -> Fmt.epr "<<< %a is not a valid object.\n%!" Fpath.pp path ; repl () in
    repl ()
  | [ x ] -> [ x ]
  | res ->
    Fmt.pr "[%s] wants to be resolved with:\n%!" name ;
    let pp i = function
      | `Caml x ->
        let copts = Resolver.Artifact.copts x in
        let cobjs = Resolver.Artifact.cobjs x in
        Fmt.pr "%3d. %a.\n%!" i Fpath.pp (Resolver.Artifact.path x) ;
        if List.length copts > 0
        then Fmt.pr "    copts: %a\n%!" Fmt.(Dump.list string) copts ;
        if List.length cobjs > 0
        then Fmt.pr "    cobjs: %a\n%!" Fmt.(Dump.list string) cobjs
      | `Object x ->
        Fmt.pr "%3d. %a.\n%!" i Fpath.pp x in
    List.iteri pp res ;

    let rec repl () =
      Fmt.pr ">>> %!" ;
      match int_of_string (input_line stdin) with
      | n ->
        if n >= 0 && n < List.length res
        then [ List.nth res n ]
        else ( Fmt.epr "<<< Bad number.\n%! " ; repl () )
      | exception (Invalid_argument _) ->
        Fmt.epr "<<< Bad input.\n%!" ; repl () in
    repl ()

let ident x = Resolver.Artifact.name x, Resolver.Artifact.kind x, Resolver.Artifact.path x
let pp_artifact = Resolver.Artifact.pp

module Resolver = Resolver.Make(Gamma)

let run () =
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  let argv = Array.to_list argv in
  let argv = List.map Fpath.v argv in
  List.fold_left (function
      | Error _ as err -> fun _ -> err
      | Ok r -> fun x -> recognize_argument x >>| fun x -> x :: r)
    (R.ok []) argv >>= fun argv ->
  let paths, objects = List.partition (function `Lookup _ -> true | _ -> false) argv in
  populate (List.map (function `Lookup x -> x | _ -> assert false) paths) >>= fun gamma ->
  Fmt.pr "[%a] %d artifacts loaded.\n%!" Fmt.(styled `Blue string) "INF" (Gamma.cardinal gamma) ;
  let srcs = List.map (function `Root x -> List.map (fun x -> `Caml x) x | _ -> assert false) objects in
  Resolver.resolve ident ~select ~srcs gamma |> function
  | Ok _ as res -> res
  | Error _ as err -> err

let pp_error ppf = function
  | #Caml_objinfo.error as err -> Caml_objinfo.pp_error ppf err
  | `Unresolved_dependency ((key, _, _), (name, digest)) ->
    Fmt.pf ppf "Unresolved dependency. %s wants %s (%a)"
      key name Fmt.(Dump.option Digest.pp) digest

let () = match run () with
  | Ok _ -> ()
  | Error err -> Fmt.epr "[%a] %a.\n%!" Fmt.(styled `Red string) "ERR" pp_error err
