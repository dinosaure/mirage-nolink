open Rresult

let len_magic_number = String.length Config.cmo_magic_number

let split name deps =
  let rec go digest acc = function
    | [] ->
      ( match digest with
        | None -> Fmt.failwith "self-digest for %s not found" name
        | Some digest -> (digest, List.rev acc) )
    | (name', digest') :: deps when name = name' ->
      ( match digest with
        | None -> go digest' acc deps
        | Some digest -> match digest' with
          | None -> go (Some digest) acc deps
          | Some digest' when digest = digest' -> go (Some digest) acc deps
          | Some digest' ->
            Fmt.failwith "multiple self-digest for %s (%s and %s)"
              name (Digest.to_hex digest) (Digest.to_hex digest') )
    | x :: deps -> go digest (x :: acc) deps in
  go None [] deps

let name_of_fpath fpath =
  let basename = Fpath.basename fpath in
  match Astring.String.cut ~rev:true ~sep:"." basename with
  | None -> basename
  | Some (name, _) -> name

type caml =
  [ `Cmo of Cmo_format.compilation_unit
  | `Cma of Fpath.t * Cmo_format.library
  | `Cmx of Cmx_format.unit_infos * Digest.t
  | `Cmxa of Fpath.t * Cmx_format.library_infos
  | `Cmi of Cmi_format.cmi_infos
  | `Cmti of Cmi_format.cmi_infos
  | `Cmt of Cmt_format.cmt_infos ]

let info path =
  Bos.OS.File.exists path >>= function
  | false -> R.error (`Not_found path)
  | true ->
    let f ic () =
      let magic = really_input_string ic len_magic_number in
      if magic = Config.cmo_magic_number
      then ( let pos = input_binary_int ic in
             seek_in ic pos ;
             let res = (input_value ic : Cmo_format.compilation_unit) in
             `Cmo res )
      else if magic = Config.cma_magic_number
      then ( let pos = input_binary_int ic in
             seek_in ic pos ;
             let res = (input_value ic : Cmo_format.library) in
             `Cma (path, res) )
      else if magic = Config.cmi_magic_number
      then ( let cmi = Cmi_format.read_cmi (Fpath.to_string path) in
             `Cmi cmi )
      else if magic = Config.cmt_magic_number
      then ( match Cmt_format.read (Fpath.to_string path) with
           | Some cmi, _ -> `Cmti cmi
           | _, Some cmt -> `Cmt cmt
           | None, None -> `Invalid_object )
      else if magic = Config.cmx_magic_number 
      then ( let res = (input_value ic : Cmx_format.unit_infos) in
             let crc = Digest.input ic in
             `Cmx (res, crc) )
      else if magic = Config.cmxa_magic_number
      then ( let res = (input_value ic : Cmx_format.library_infos) in
             `Cmxa (path, res) )
      else `Invalid_object in
    match Bos.OS.File.with_ic path f () with
    | Ok (#caml as v) -> R.ok v
    | Ok `Invalid_object -> R.error (`Invalid_object path)
    | Error err -> Error err

    | exception (Cmi_format.Error exn) ->
      R.error_msgf "%a" Cmi_format.report_error exn
    | exception Cmt_format.(Error (Not_a_typedtree s)) ->
      R.error_msgf "Not a typed-tree: %s" s
    | exception (Failure e) ->
      R.error_msg e
    | exception (Sys_error e) ->
      R.error_msg e
    | exception End_of_file ->
      R.error_msgf "Unexpected end-of-file on %a" Fpath.pp path

module type S = sig
  type t

  val name : t -> string
  val iface_digest : t -> Digest.t
  val iface_deps : t -> (string * Digest.t option) list
end

module CMI = struct
  type t =
    { name : string
    ; iface_digest : Digest.t
    ; iface_deps : (string * Digest.t option) list }

  let name { name; _ } = name
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps

  let of_cmi (`Cmi cmi) =
    let name = cmi.Cmi_format.cmi_name in
    let iface_digest, iface_deps = split name cmi.Cmi_format.cmi_crcs in
    { name; iface_digest; iface_deps; }
end

module CMTI = struct
  type t =
    { name : string
    ; iface_digest : Digest.t
    ; iface_deps : (string * Digest.t option) list }

  let name { name; _ } = name
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps

  let of_cmti (`Cmti cmti) = 
    let name = cmti.Cmi_format.cmi_name in
    let iface_digest, iface_deps = split name cmti.Cmi_format.cmi_crcs in
    { name; iface_digest; iface_deps; }
end

module CMO = struct
  type t =
    { name : string
    ; iface_digest : Digest.t
    ; iface_deps : (string * Digest.t option) list }

  let name { name; _ } = name
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps

  let of_cmo (`Cmo cmo) =
    let name = cmo.Cmo_format.cu_name in
    let iface_digest, iface_deps =
      split name cmo.Cmo_format.cu_imports in
    { name; iface_digest; iface_deps; }
end

module CMT = struct
  type t =
    { name : string
    ; iface_digest : Digest.t
    ; iface_deps : (string * Digest.t option) list }

  let name { name; _ } = name
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps

  let of_cmt (`Cmt cmt) =
    let name = cmt.Cmt_format.cmt_modname in
    let iface_digest, iface_deps = split name cmt.Cmt_format.cmt_imports in
    { name; iface_digest; iface_deps; }
end

module CMA = struct
  type t =
    { name : string
    ; cmos : CMO.t list
    ; custom : bool
    ; custom_cobjs : string list
    ; custom_copts : string list
    ; dllibs : string list }

  let of_cma (`Cma (fpath, cma)) =
    let name = name_of_fpath fpath in
    let cmos = List.map (fun cmo -> CMO.of_cmo (`Cmo cmo)) cma.Cmo_format.lib_units in
    let custom = cma.Cmo_format.lib_custom in
    let custom_cobjs = cma.Cmo_format.lib_ccobjs in
    let custom_copts = cma.Cmo_format.lib_ccopts in
    let dllibs = cma.Cmo_format.lib_dllibs in
    { name; cmos; custom; custom_cobjs; custom_copts; dllibs; }

  let name { name; _ } = name
  let cmos { cmos; _ } = cmos
  let custom { custom; _ } = custom
  let custom_cobjs { custom_cobjs; _ } = custom_cobjs
  let custom_copts { custom_copts; _ } = custom_copts
  let dllibs { dllibs; _ } = dllibs
end

module CMX = struct
  type t =
    { name : string
    ; digest : Digest.t
    ; iface_digest : Digest.t
    ; iface_deps : (string * Digest.t option) list
    ; cmx_deps : (string * Digest.t option) list }

  let of_cmx (`Cmx (cmx, digest)) =
    let name = cmx.Cmx_format.ui_name in
    let iface_digest, iface_deps = split name cmx.Cmx_format.ui_imports_cmi in
    let cmx_deps = cmx.Cmx_format.ui_imports_cmx in
    { name; digest; iface_digest; iface_deps; cmx_deps; }

  let name { name; _ } = name
  let digest { digest; _ } = digest
  let iface_digest { iface_digest; _ } = iface_digest
  let iface_deps { iface_deps; _ } = iface_deps
  let cmx_deps { cmx_deps; _ } = cmx_deps
end

module CMXA = struct
  type t =
    { name : string
    ; cmxs : CMX.t list
    ; cobjs : string list
    ; copts : string list }

  let of_cmxa (`Cmxa (fpath, cmxa)) =
    let name = name_of_fpath fpath in
    let cmxs = List.map (fun x -> CMX.of_cmx (`Cmx x)) cmxa.Cmx_format.lib_units in
    let cobjs = cmxa.Cmx_format.lib_ccobjs in
    let copts = cmxa.Cmx_format.lib_ccopts in
    { name; cmxs; cobjs; copts; }

  let name { name; _ } = name
  let cmxs { cmxs; _ } = cmxs
  let cobjs { cobjs; _ } = cobjs
  let copts { copts; _ } = copts
end

type t =
  | CMO of CMO.t
  | CMI of CMI.t
  | CMX of CMX.t
  | CMA of CMA.t
  | CMT of CMT.t
  | CMXA of CMXA.t
  | CMTI of CMTI.t

type error =
  [ `Invalid_object of Fpath.t
  | `Not_found of Fpath.t
  | `Msg of string ]

let pp_error ppf = function
  | `Invalid_object path ->
    Fmt.pf ppf "%a is an invalid object" Fpath.pp path
  | `Not_found path ->
    Fmt.pf ppf "%a does not exist" Fpath.pp path
  | `Msg err -> Fmt.string ppf err

let info path =
  info path >>| function
  | `Cmo _ as cmo -> CMO (CMO.of_cmo cmo)
  | `Cmi _ as cmi -> CMI (CMI.of_cmi cmi)
  | `Cmx _ as cmx -> CMX (CMX.of_cmx cmx)
  | `Cma _ as cma -> CMA (CMA.of_cma cma)
  | `Cmt _ as cmt -> CMT (CMT.of_cmt cmt)
  | `Cmxa _ as cmxa -> CMXA (CMXA.of_cmxa cmxa)
  | `Cmti _ as cmti -> CMTI (CMTI.of_cmti cmti)
