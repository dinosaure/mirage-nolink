open Rresult

module Artifact = struct
  type kind =
    | CMI
    | CMO
    | CMX
  
  type dep = string * Digest.t option
  
  type t =
    { kind : kind
    ; name : string
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
      { kind; name= O.name o; path; iface_digest= Some (O.iface_digest o)
      ; iface_deps= O.iface_deps o; in_archive= false
      ; cobjs= []
      ; copts= []
      ; m= lazy [ res ] } in
    Ok [ res ]
  
  let of_cma cma path =
    let open Caml_objinfo in
    let cmos = CMA.cmos cma in
    let of_cmo cmo m =
      { kind= CMO; name= CMO.name cmo; path; iface_digest= Some (CMO.iface_digest cmo)
      ; iface_deps= CMO.iface_deps cmo; in_archive= true
      ; cobjs= CMA.custom_cobjs cma
      ; copts= CMA.custom_copts cma
      ; m } in
    let rec m = lazy (List.map (fun cmo -> of_cmo cmo m) cmos) in
    Ok (Lazy.force m)
  
  let of_cmxa cmxa path =
    let open Caml_objinfo in
    let cmxs = CMXA.cmxs cmxa in
    let of_cmx cmx m =
      { kind= CMX; name= CMX.name cmx; path; iface_digest= Some (CMX.iface_digest cmx)
      ; iface_deps= CMX.iface_deps cmx; in_archive= true
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

module SMap = Map.Make(String)
module DMap = Map.Make(Digest)

module PMap = Map.Make(struct
    type t = Fpath.t * string

    let compare (fa, a) (fb, b) =
      let res = Fpath.compare fa fb in
      if res = 0 then String.compare a b
      else res
  end)

module type GAMMA = sig 
  type t
  type 'a select

  val of_name :
    string ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list

  val of_digest : Digest.t -> t -> Artifact.t list
  val of_dep :
    Artifact.dep ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list

  val cardinal : t -> int
  val populate : Artifact.t list -> t -> t
  val empty : t

  val add_object :
    name:string ->
    Fpath.t ->
    t ->
    (t, [> `Invalid_object of Fpath.t
        |  `Msg of string
        |  `Not_found of Fpath.t ]) result

  val find :
    select:('a ->
            Artifact.dep ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list ->
            [ `Caml of Artifact.t
            | `Object of Fpath.t ] list select) ->
    'a ->
    Artifact.dep ->
    t ->
    [ `Caml of Artifact.t | `Object of Fpath.t ] list
end

module Gamma (Selective : sig include Selective.S val run : 'a t -> 'a end)
  : GAMMA with type 'a select = 'a Selective.t
= struct
  type t =
    { nmap : [ `Caml of Artifact.t | `Object of Fpath.t ] list SMap.t
    ; dmap : Artifact.t list DMap.t }

  type 'a select = 'a Selective.t

  let of_name name t = match SMap.find name t.nmap with
    | exception Not_found -> []
    | res -> res

  let of_digest digest t = match DMap.find digest t.dmap with
    | exception Not_found -> []
    | res -> res

  let cardinal t =
    let res = ref 0 in
    SMap.iter (fun _ vs -> res := !res + List.length vs) t.nmap ;
    !res

  let populate l t =
    let add_name k v m = match SMap.find k m with
      | exception Not_found -> SMap.add k [ `Caml v ] m
      | vs -> SMap.add k (`Caml v :: vs) m in
    let add_digest k v m = match DMap.find k m with
      | exception Not_found -> DMap.add k [ v ] m
      | vs -> DMap.add k (v :: vs) m in
    let rec go nmap dmap = function
      | [] -> { nmap; dmap; }
      | x :: r ->
        let nmap = add_name (Artifact.name x) x nmap in
        let dmap = match Artifact.iface_digest x with
          | None -> dmap
          | Some digest -> add_digest digest x dmap in
        go nmap dmap r in
    go t.nmap t.dmap l

  let add_object ~name path t =
    Bos.OS.File.exists path >>= function
    | false -> R.error (`Not_found path)
    | true -> match Fpath.get_ext path with
      | ".a" ->
        let nmap =
          try let vs = SMap.find name t.nmap in
              SMap.add name (`Object path :: vs) t.nmap
          with Not_found -> SMap.add name [ `Object path ] t.nmap in
        R.ok { nmap; dmap= t.dmap; }
      | _ -> R.error (`Invalid_object path)

  let empty =
    { nmap= SMap.empty
    ; dmap= DMap.empty }

  let of_dep (name, digest) t = match digest with
    | None -> of_name name t
    | Some digest -> List.map (fun x -> `Caml x) (of_digest digest t)

  let find ~select who dep t =
    let res = of_dep dep t in
    let filter = function
      | `Caml x -> Artifact.in_archive x
      | `Object _ -> true in
    match List.filter filter res with
    | [] -> Selective.run (select who dep res)
    | [ x ] -> [ x ]
    | ars -> Selective.run (select who dep ars)
end

type t = Artifact.t SMap.t
type deps = Artifact.dep list 

let add ident t depss acc x =
  let rec go t depss = function
    | [] -> (t, depss) :: acc
    | x :: r ->
      match SMap.find (Artifact.name x) t with
      | x' ->
        if Fpath.equal (Artifact.path x) (Artifact.path x')
        then go t depss r else (* NOTE: assert false? *) acc
      | exception Not_found ->
        let exception Exit in
        let add_dependency acc (n, digest) = match SMap.find n t with
          | exception Not_found -> (n, digest) :: acc
          | y' -> match digest, Artifact.iface_digest y' with
            | Some a, Some b when not (Digest.equal a b) -> raise Exit
            | _ -> acc in
        match List.fold_left add_dependency [] (Artifact.iface_deps x) with
        | exception Exit -> acc
        | deps ->
          let name = Artifact.name x in
          let ident = ident x in
          go (SMap.add name x t) ((ident, deps) :: depss) r in
  match x with
  | `Caml x -> go t depss (Artifact.m x)
  | `Object _ -> acc

let of_sources ident acc srcs =
  let rec go t depss = function
    | [] -> (t, depss) :: acc
    | x :: r -> match add ident t depss [] x with
      | [] -> acc
      | [ (t, depss) ] -> go t depss r
      | _ -> assert false in
  go SMap.empty [] srcs

module Make (Gamma : GAMMA) = struct
  type 'key error =
    [ `Msg of string
    | `Unresolved_dependency of ('key * Artifact.dep) ]

  let resolve ident ~select ~srcs gamma =
    let rec resolve acc pending = match pending with
      | [] ->
        let acc = List.find_all (fun g -> not (SMap.is_empty g)) acc in R.ok acc
      | (t, depss) :: r -> go acc r t depss
    and go acc next t depss =
      match depss with
      | [] -> R.ok []
      | (key, deps) :: rest ->
        match deps, rest with
        | [], [] -> resolve (t :: acc) next
        | [], rest -> go acc next t rest
        | (n, digest) :: deps, rest -> match SMap.find n t with
          | artifact ->
            ( match digest, Artifact.iface_digest artifact with
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
end
