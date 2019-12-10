open Rresult
open Resolver
open Mirage_nolink

let target : [ `Unix | `Freestanding | `Xen ] ref = ref `Unix
let impl : [ `C | `OCaml ] ref = ref `C

let stdlib = Caml_name.v "Stdlib"
and stdlib_resolver _who _digest res =
  let is_stdlib = function
    | `Object _ -> false
    | `Caml x -> match Fpath.basename (Caml_obj.path x) with
      | "stdlib.cmxa" -> true
      | _ -> false in
  try [ List.find is_stdlib res ], []
  with Not_found -> [], []

let () = Mirage_nolink.register ~name:stdlib stdlib_resolver
let () = Mirage_nolink.register ~name:(Caml_name.v "CamlinternalFormatBasics") stdlib_resolver

let rec z = Caml_name.v "Z"
and gmp = Caml_name.v "Gmp"
and z_resolver _who _digest res =
  match !target with
  | `Unix ->
    (* XXX(dinosaure): try to find [libgmp.a]? *)
    take ~kind:Caml_obj.CMX ~archive:true ~name:(Caml_name.v "Zarith") res, []
  | `Xen | `Freestanding as target->
    let res =
      Bos.OS.Cmd.(run_out Bos.Cmd.(v "opam" % "config" % "var" % "lib") |> to_string)
      >>| Fpath.v
      >>= fun lib ->
      ( match target with
        | `Xen -> Obj.of_path ~name:gmp Fpath.(lib / "gmp-xen" / "libgmp_xen.a")
        | `Freestanding -> Obj.of_path ~name:gmp Fpath.(lib / "gmp-freestanding" / "libgmp_freestanding.a") )
      >>| fun static -> take ~kind:Caml_obj.CMX ~archive:false ~name:z res, [ `Object static ] in
    match res with
    | Ok res -> res
    | Error _ -> [], []

let () = Mirage_nolink.register ~name:z z_resolver

let rec native = Caml_name.v "Native"
and native_resolver _who _digest _res =
  let res =
    Bos.OS.Cmd.(run_out Bos.Cmd.(v "opam" % "config" % "var" % "lib") |> to_string)
    >>| Fpath.v
    >>= fun lib -> match !target with
    | `Unix -> Obj.of_path ~name:native Fpath.(lib / "nocrypto" / "libnocrypto_stubs.a")
    | `Xen -> Obj.of_path ~name:native Fpath.(lib / "nocrypto" / "libnocrypto_stubs+mirage-xen.a")
    | `Freestanding -> Obj.of_path ~name:native Fpath.(lib / "nocrypto" / "libnocrypto_stubs+mirage-freestanding.a") in
  match res with
  | Ok x -> [ `Object x ], []
  | Error _ -> [], []

let () = Mirage_nolink.register ~name:native native_resolver

let digestif = Caml_name.v "Digestif"
let digestif_resolver _who _digest res =
  match !target, !impl with
  | _, `OCaml ->
    let is_digestif_ocaml = function
      | `Object _ -> false
      | `Caml x -> match Fpath.basename (Caml_obj.path x) with
        | "digestif_ocaml.cmxa" -> true
        | _ -> false in
    List.filter is_digestif_ocaml res, []
  | `Unix, `C ->
    let is_digestif_c = function
      | `Object _ -> false
      | `Caml x -> match Fpath.basename (Caml_obj.path x) with
        | "digestif_c.cmxa" -> Caml_obj.in_archive x && Caml_obj.kind x = Caml_obj.CMX
        | _ -> false in
    List.filter is_digestif_c res, []
  | (`Freestanding | `Xen), `C ->
    let is_digestif_c = function
      | `Object _ -> false
      | `Caml x -> match Fpath.basename (Caml_obj.path x) with
        | "digestif_c.cmx" -> not (Caml_obj.in_archive x) && Caml_obj.kind x = Caml_obj.CMX
        | _ -> false in
    List.filter is_digestif_c res, []

let () = Mirage_nolink.register ~name:digestif digestif_resolver

let rakia = Caml_name.v "Rakia"
and rakia_resolver _who _digest res =
  let is ~basename = function
    | `Object _ -> false
    | `Caml x -> Fpath.basename (Caml_obj.path x) = basename in
  match !target with
  | `Unix -> List.filter (is ~basename:"rakia_unix.cmxa") res, []
  | `Freestanding -> List.filter (is ~basename:"rakia_freestanding.cmxa") res, []
  | `Xen -> List.filter (is ~basename:"rakia_xen.cmxa") res, []

let () = Mirage_nolink.register ~name:rakia rakia_resolver

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
            ( match Caml_obj.of_path hd with
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

let recognize_argument x =
  Bos.OS.Path.exists x >>= function
  | false -> R.error_msgf "%a does not exist" Fpath.pp x
  | true -> Bos.OS.Path.stat x >>= fun stat -> match stat.Unix.st_kind with
    | Unix.S_REG -> Caml_obj.of_path x >>= fun x -> R.ok (`Root x)
    | Unix.S_DIR -> R.ok (`Lookup x)
    | _ -> R.error_msgf "Invalid path (neither file nor directory): %a" Fpath.pp x

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
  Mirage_nolink.resolve ~kind:Caml_obj.CMX ~srcs gamma |> function
  | Ok _ as res -> res
  | Error _ as err -> err

let () = match run () with
  | Ok ts ->
    let f k = function
      | `Caml x -> Fmt.pr "%a: %a\n%!" Caml_obj.pp x Caml_name.pp k
      | `Object x -> Fmt.pr "%a: %a\n%!" Obj.pp x Caml_name.pp k in
    Fmt.pr ">>> Resolution done.\n%!" ;
    List.iter (Resolver.iter ~f) ts
  | Error (#Mirage_nolink.error as err)-> Fmt.epr "[%a] %a.\n%!" Fmt.(styled `Red string) "ERR" Mirage_nolink.pp_error err
  | Error (#Caml_objinfo.error as err)-> Fmt.epr "[%a] %a.\n%!" Fmt.(styled `Red string) "ERR" Caml_objinfo.pp_error err
