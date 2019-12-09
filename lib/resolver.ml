open Rresult

module Caml_name : sig
  type t = private string

  val unsafe : string -> t
  val to_string : t -> string
  val pp : t Fmt.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t = string

  let unsafe x = x
  let to_string x = x
  let pp = Fmt.string
  let compare = String.compare
  let equal = String.equal
end

type error = [ `Not_found of Fpath.t | `Invalid_object of Fpath.t | `Msg of string ]

let pp_error ppf = function
  | `Not_found path ->
    Fmt.pf ppf "%a does not exist" Fpath.pp path
  | `Invalid_object path ->
    Fmt.pf ppf "%a is an invalid object" Fpath.pp path
  | `Msg err -> Fmt.string ppf err

module Caml_obj : sig
  type kind = CMI | CMO | CMX
  type dep = Caml_name.t * Digest.t option
  type t

  val pp : t Fmt.t
  val kind : t -> kind 
  val name : t -> Caml_name.t
  val path : t -> Fpath.t
  val m : t -> t list

  val iface_digest : t -> Digest.t option
  val iface_deps : t -> dep list
  val in_archive : t -> bool
  val cobjs : t -> string list
  val copts : t -> string list

  val of_path : Fpath.t -> (t list, [> Caml_objinfo.error ]) result
  val to_dep : t -> dep
end = struct
  type kind =
    | CMI
    | CMO
    | CMX
 
  type dep = Caml_name.t * Digest.t option
  
  type t =
    { kind : kind
    ; name : Caml_name.t
    ; path : Fpath.t
    ; iface_digest : Digest.t option
    ; iface_deps : dep list
    ; in_archive : bool
    ; cobjs : string list
    ; copts : string list
    ; m : t list Lazy.t }

  let pp ppf t = Fpath.pp ppf t.path

  let kind { kind; _ } = kind
  let name { name; _ } = name
  let path { path; _ } = path
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps
  let in_archive { in_archive; _ } = in_archive
  let m { m; _ } = Lazy.force m
  let cobjs { cobjs; _ } = cobjs
  let copts { copts; _ } = copts
  
  let to_dep { name; iface_digest; _ } = (name, iface_digest)
  
  let of_s
    : type v. (module Caml_objinfo.S with type t = v) -> v -> kind -> Fpath.t -> (t list, [> R.msg ]) result
    = fun (module O) o kind path ->
    let rec res =
      let iface_deps o =
        List.map (fun (k, v) -> Caml_name.unsafe k, v) (O.iface_deps o) in
      { kind; name= Caml_name.unsafe (O.name o); path
      ; iface_digest= Some (O.iface_digest o); iface_deps= (iface_deps o)
      ; in_archive= false
      ; cobjs= []
      ; copts= []
      ; m= lazy [ res ] } in
    Ok [ res ]
  
  let of_cma cma path =
    let open Caml_objinfo in
    let cmos = CMA.cmos cma in
    let of_cmo cmo m =
      let iface_deps o =
        List.map (fun (k, v) -> Caml_name.unsafe k, v) (CMO.iface_deps o) in
      { kind= CMO; name= Caml_name.unsafe (CMO.name cmo); path
      ; iface_digest= Some (CMO.iface_digest cmo); iface_deps= iface_deps cmo
      ; in_archive= true
      ; cobjs= CMA.custom_cobjs cma
      ; copts= CMA.custom_copts cma
      ; m } in
    let rec m = lazy (List.map (fun cmo -> of_cmo cmo m) cmos) in
    Ok (Lazy.force m)
  
  let of_cmxa cmxa path =
    let open Caml_objinfo in
    let cmxs = CMXA.cmxs cmxa in
    let of_cmx cmx m =
      let iface_deps o =
        List.map (fun (k, v) -> Caml_name.unsafe k, v) (CMX.iface_deps o) in
      { kind= CMX; name= Caml_name.unsafe (CMX.name cmx); path
      ; iface_digest= Some (CMX.iface_digest cmx); iface_deps= iface_deps cmx
      ; in_archive= true
      ; cobjs= CMXA.cobjs cmxa
      ; copts= CMXA.copts cmxa
      ; m } in
    let rec m = lazy (List.map (fun cmx -> of_cmx cmx m) cmxs) in
    Ok (Lazy.force m)
  
  let of_path path =
    Caml_objinfo.info path >>= function
    | CMI cmi -> of_s (module Caml_objinfo.CMI) cmi CMI path
    | CMO cmo -> of_s (module Caml_objinfo.CMO) cmo CMO path
    | CMT cmt -> of_s (module Caml_objinfo.CMT) cmt CMO path
    | CMX cmx -> of_s (module Caml_objinfo.CMX) cmx CMX path
    | CMTI cmti -> of_s (module Caml_objinfo.CMTI) cmti CMI path
    | CMXA cmxa -> of_cmxa cmxa path
    | CMA cma -> of_cma cma path
end

module Obj : sig
  type t

  val path : t -> Fpath.t
  val name : t -> Caml_name.t
  val pp : t Fmt.t

  val of_path : name:Caml_name.t -> Fpath.t -> (t, [> error ]) result
end = struct
  type t =
    { path : Fpath.t
    ; name : Caml_name.t }

  let name { name; _ } = name
  let path { path; _ } = path

  let of_path ~name path =
    Bos.OS.File.exists path >>= function
    | false -> R.error (`Not_found path)
    | true -> match Fpath.get_ext path with
      | ".a" -> R.ok { name; path; }
      | _ -> R.error (`Invalid_object path)

  let pp ppf { path; _ } = Fpath.pp ppf path
end

module CMap = Map.Make(Caml_name)
module DMap = Map.Make(Digest)

module PMap = Map.Make(struct
    type t = Fpath.t * string

    let compare (fa, a) (fb, b) =
      let res = Fpath.compare fa fb in
      if res = 0 then String.compare a b
      else res
  end)

type artifact = [ `Caml of Caml_obj.t | `Object of Obj.t ]

module type GAMMA = sig 
  type t
  type 'a select

  val of_name : Caml_name.t -> t -> artifact list
  val of_digest : Digest.t -> t -> Caml_obj.t list
  val of_dep : Caml_obj.dep -> t -> artifact list

  val cardinal : t -> int
  val populate : Caml_obj.t list -> t -> t
  val add_object : name:Caml_name.t -> Fpath.t -> t -> (t, error) result
  val empty : t

  val find : select:('a -> Caml_obj.dep -> (artifact list -> artifact list) select) -> 'a -> Caml_obj.dep -> t -> artifact list
end

module Gamma (Selective : sig include Selective.S val run : 'a t -> 'a end)
  : GAMMA with type 'a select = 'a Selective.t
= struct
  type t =
    { nmap : artifact list CMap.t
    ; dmap : Caml_obj.t list DMap.t }

  type 'a select = 'a Selective.t

  let of_name name t = match CMap.find name t.nmap with
    | exception Not_found -> []
    | res -> res

  let of_digest digest t = match DMap.find digest t.dmap with
    | exception Not_found -> []
    | res -> res

  let cardinal t =
    let res = ref 0 in
    CMap.iter (fun _ vs -> res := !res + List.length vs) t.nmap ;
    !res

  let populate l t =
    let add_name k v m = match CMap.find k m with
      | exception Not_found -> CMap.add k [ `Caml v ] m
      | vs -> CMap.add k (`Caml v :: vs) m in
    let add_digest k v m = match DMap.find k m with
      | exception Not_found -> DMap.add k [ v ] m
      | vs -> DMap.add k (v :: vs) m in
    let rec go nmap dmap = function
      | [] -> { nmap; dmap; }
      | x :: r ->
        let nmap = add_name (Caml_obj.name x) x nmap in
        let dmap = match Caml_obj.iface_digest x with
          | None -> dmap
          | Some digest -> add_digest digest x dmap in
        go nmap dmap r in
    go t.nmap t.dmap l

  let add_object ~name path t =
    Obj.of_path ~name path >>= fun x ->
    let nmap =
      try let vs = CMap.find name t.nmap in
          CMap.add name (`Object x :: vs) t.nmap
      with Not_found -> CMap.add name [ `Object x ] t.nmap in
    R.ok { nmap; dmap= t.dmap; }

  let empty =
    { nmap= CMap.empty
    ; dmap= DMap.empty }

  let of_dep (name, digest) t = match digest with
    | None -> of_name name t
    | Some digest -> List.map (fun x -> `Caml x) (of_digest digest t)

  let no_copts = function
    | `Caml x -> (Caml_obj.copts x) = []
    | `Object _ -> true
  let no_cobjs = function
    | `Caml x -> (Caml_obj.cobjs x) = []
    | `Object _ -> true

  let find ~select who dep t =
    let res = of_dep dep t in
    let filter = function
      | `Caml x -> Caml_obj.in_archive x
      | `Object _ -> true (* XXX(dinosaure): [*.a] is an archive. *) in
    match List.filter filter res with
    | [] -> Selective.run (select who dep) res
    | [ x ] ->
      if no_copts x && no_cobjs x
      then [ x ]
      else Selective.run (select who dep) res
    | ars ->
      if List.for_all no_copts ars && List.for_all no_cobjs ars
      then Selective.run (select who dep) ars
      else Selective.run (select who dep) res
end

type t = artifact CMap.t
type deps = Caml_obj.dep list 

let add ident t depss acc x =
  let rec go t depss = function
    | [] -> (t, depss) :: acc
    | x :: r ->
      match CMap.find (Caml_obj.name x) t with
      | `Object _ -> go t depss r
      | `Caml x' ->
        if Fpath.equal (Caml_obj.path x) (Caml_obj.path x')
        then go t depss r else (* NOTE: assert false? *) acc
      | exception Not_found ->
        let exception Exit in
        let add_dependency acc (n, digest) = match CMap.find n t with
          | exception Not_found -> (n, digest) :: acc
          | `Object _ -> acc
          | `Caml y' -> match digest, Caml_obj.iface_digest y' with
            | Some a, Some b when not (Digest.equal a b) -> raise Exit
            | _ -> acc in
        match List.fold_left add_dependency [] (Caml_obj.iface_deps x) with
        | exception Exit -> acc
        | deps ->
          let name = Caml_obj.name x in
          let ident = ident x in
          go (CMap.add name (`Caml x) t) ((ident, deps) :: depss) r in
  match x with
  | `Caml x -> go t depss (Caml_obj.m x)
  | `Object x -> (CMap.add (Obj.name x) (`Object x) t, depss) :: acc

let of_sources ident acc srcs =
  let rec go t depss = function
    | [] -> (t, depss) :: acc
    | x :: r -> match add ident t depss [] x with
      | [] -> acc
      | [ (t, depss) ] -> go t depss r
      | _ -> assert false in
  go CMap.empty [] srcs

module Make (Gamma : GAMMA) = struct
  type 'key error =
    [ `Msg of string
    | `Unresolved_dependency of ('key * Caml_obj.dep) ]

  let resolve ident ~select ~srcs gamma =
    let rec resolve acc pending = match pending with
      | [] ->
        let acc = List.find_all (fun g -> not (CMap.is_empty g)) acc in R.ok acc
      | (t, depss) :: r -> go acc r t depss
    and go acc next t depss =
      match depss with
      | [] -> R.ok []
      | (key, deps) :: rest ->
        match deps, rest with
        | [], [] -> resolve (t :: acc) next
        | [], rest -> go acc next t rest
        | (n, digest) :: deps, rest -> match CMap.find n t with
          | `Object _ -> go acc next t ((key, deps) :: rest)
          | `Caml res ->
            ( match digest, Caml_obj.iface_digest res with
              | Some a, Some b when not (Digest.equal a b) ->
                resolve acc next
              | _ -> go acc next t ((key, deps) :: rest) )
          | exception Not_found ->
            match Gamma.find ~select key (n, digest) gamma with
            | [] -> R.error (`Unresolved_dependency (key, (n, digest)))
            | res -> match List.fold_left (add ident t ((key, deps) :: rest)) [] res with
              | [] -> resolve acc next
              | (t, deps) :: next' ->
                let next = List.rev_append next' next in
                go acc next t deps in
    let pending = List.fold_left (of_sources ident) [] srcs in
    resolve [] pending

  let iter ~f t = CMap.iter (fun k x -> f k x) t
end
