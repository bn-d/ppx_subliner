open Ppxlib

let prefix = "subliner."
let prefix_len = String.length prefix

type 'a level = Derived of 'a | General of 'a | Prefixed of 'a

module Level = struct
  let join level_opt level =
    match (level_opt, level) with
    | None, _
    | Some (Derived _), _
    | Some (General _), General _
    | _, Prefixed _ ->
        Some level
    | _ -> level_opt

  let get = function Derived a | General a | Prefixed a -> a

  let map f = function
    | Derived a -> Derived (f a)
    | General a -> General (f a)
    | Prefixed a -> Prefixed (f a)

  let general opt = Option.map (fun v -> General v) opt
  let prefixed opt = Option.map (fun v -> Prefixed v) opt
end

let parse_impl
    ~empty
    ~map
    ~tag_of_string
    ~update_field_of_tag
    (attrs : attributes) =
  let tag_level_of_attr_name { txt = name; loc } =
    let tag = tag_of_string name in
    match name with
    | _ when Option.is_some tag -> Level.general tag
    | "ocaml.doc" -> Some (Derived `doc)
    | _ when Utils.string_starts_with ~prefix name ->
        let len = String.length name in
        let name = String.sub name prefix_len (len - prefix_len) in
        let tag = tag_of_string name in
        if Option.is_some tag then
          Level.prefixed tag
        else
          Error.attribute_name ~loc name
    | _ -> None
  in
  attrs
  |> List.fold_left
       (fun acc attr ->
         let loc = attr.attr_loc in
         tag_level_of_attr_name attr.attr_name
         |> function
         | None -> acc
         | Some field ->
             update_field_of_tag (Level.get field)
               (Level.map
                  (fun _ ->
                    match attr.attr_payload with
                    | PStr structure -> (loc, structure)
                    | _ -> Error.attribute_payload ~loc "attribute")
                  field)
               acc)
       empty
  |> map Level.get

let parse_single name attrs =
  let tag_of_string s = if s = name then Some `current else None
  and update_field_of_tag tag v t =
    match tag with `current -> Level.join t v | `doc -> t
  in
  parse_impl ~empty:None ~map:Option.map ~tag_of_string ~update_field_of_tag
    attrs

let to_bool =
  Option.fold ~none:false ~some:(function
    | _, [] -> true
    | loc, _ -> Error.attribute_flag ~loc)

let to_expr name = function
  | _, [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> expr
  | loc, _ -> Error.attribute_payload ~loc name

let to_expr_opt name = Option.map (to_expr name)

module Term = struct
  type 'a t = {
    (* info *)
    deprecated : 'a option;
    absent : 'a option;
    docs : 'a option;
    docv : 'a option;
    doc : 'a option;
    env : 'a option;
    env_deprecated : 'a option;
    env_docs : 'a option;
    env_doc : 'a option;
    (* named *)
    names : 'a option;
    opt_all : 'a option;
    (* positional *)
    pos : 'a option;
    pos_all : 'a option;
    pos_left : 'a option;
    pos_right : 'a option;
    rev : 'a option;
    (* as term *)
    non_empty : 'a option;
    last : 'a option;
    last_sep : 'a option;
    (* type *)
    default : 'a option;
  }
  [@@deriving make]

  let empty = make_t ()

  let map
      f
      {
        deprecated;
        absent;
        docs;
        docv;
        doc;
        env;
        env_deprecated;
        env_docs;
        env_doc;
        names;
        opt_all;
        pos;
        pos_all;
        pos_left;
        pos_right;
        rev;
        non_empty;
        last;
        last_sep;
        default;
      } =
    let f = Option.map f in
    {
      (* info *)
      deprecated = f deprecated;
      absent = f absent;
      docs = f docs;
      docv = f docv;
      doc = f doc;
      (* Cmd.Env.info *)
      env = f env;
      env_deprecated = f env_deprecated;
      env_docs = f env_docs;
      env_doc = f env_doc;
      (* named *)
      names = f names;
      opt_all = f opt_all;
      (* positional *)
      pos = f pos;
      pos_all = f pos_all;
      pos_left = f pos_left;
      pos_right = f pos_right;
      rev = f rev;
      (* as term *)
      non_empty = f non_empty;
      last = f last;
      last_sep = f last_sep;
      (* type *)
      default = f default;
    }

  let tag_of_string = function
    | "deprecated" | "deprecated_" -> Some `deprecated
    | "absent" -> Some `absent
    | "docs" -> Some `docs
    | "docv" -> Some `docv
    | "doc" -> Some `doc
    | "env" -> Some `env
    | "env.deprecated" -> Some `env_deprecated
    | "env.docs" -> Some `env_docs
    | "env.doc" -> Some `env_doc
    | "names" -> Some `names
    | "opt_all" -> Some `opt_all
    | "pos" -> Some `pos
    | "pos_all" -> Some `pos_all
    | "pos_left" -> Some `pos_left
    | "pos_right" -> Some `pos_right
    | "rev" -> Some `rev
    | "non_empty" -> Some `non_empty
    | "last" -> Some `last
    | "last.sep" -> Some `last_sep
    | "default" -> Some `default
    | _ -> None

  let update_field_of_tag tag v t =
    match tag with
    | `deprecated -> { t with deprecated = Level.join t.deprecated v }
    | `absent -> { t with absent = Level.join t.absent v }
    | `docs -> { t with docs = Level.join t.docs v }
    | `docv -> { t with docv = Level.join t.docv v }
    | `doc -> { t with doc = Level.join t.doc v }
    | `env -> { t with env = Level.join t.env v }
    | `env_deprecated ->
        { t with env_deprecated = Level.join t.env_deprecated v }
    | `env_docs -> { t with env_docs = Level.join t.env_docs v }
    | `env_doc -> { t with env_doc = Level.join t.env_doc v }
    | `names -> { t with names = Level.join t.names v }
    | `opt_all -> { t with opt_all = Level.join t.opt_all v }
    | `pos -> { t with pos = Level.join t.pos v }
    | `pos_all -> { t with pos_all = Level.join t.pos_all v }
    | `pos_left -> { t with pos_left = Level.join t.pos_left v }
    | `pos_right -> { t with pos_right = Level.join t.pos_right v }
    | `rev -> { t with rev = Level.join t.rev v }
    | `non_empty -> { t with non_empty = Level.join t.non_empty v }
    | `last -> { t with last = Level.join t.last v }
    | `last_sep -> { t with last_sep = Level.join t.last_sep v }
    | `default -> { t with default = Level.join t.default v }

  let parse attrs =
    parse_impl ~empty ~map ~tag_of_string ~update_field_of_tag attrs
end

module String_conv = struct
  type 'a t = { file : 'a option; dir : 'a option; non_dir_file : 'a option }
  [@@deriving make]

  let empty = make_t ()

  let map f { file; dir; non_dir_file } =
    let f = Option.map f in
    { file = f file; dir = f dir; non_dir_file = f non_dir_file }

  let tag_of_string = function
    | "file" -> Some `file
    | "dir" -> Some `dir
    | "non_dir_file" -> Some `non_dir_file
    | _ -> None

  let update_field_of_tag tag v t =
    match tag with
    | `file -> { t with file = Level.join t.file v }
    | `dir -> { t with dir = Level.join t.dir v }
    | `non_dir_file -> { t with non_dir_file = Level.join t.non_dir_file v }
    | `doc -> t

  let parse attrs =
    parse_impl ~empty ~map ~tag_of_string ~update_field_of_tag attrs
end

module Sep_conv = struct
  let parse = parse_single "sep"
end

module Cmd_info = struct
  type 'a t = {
    deprecated : 'a option;
    man_xrefs : 'a option;
    man : 'a option;
    envs : 'a option;
    exits : 'a option;
    sdocs : 'a option;
    docs : 'a option;
    doc : 'a option;
    version : 'a option;
    name : 'a option;
  }
  [@@deriving make]

  let empty = make_t ()

  let map
      f
      {
        deprecated;
        man_xrefs;
        man;
        envs;
        exits;
        sdocs;
        docs;
        doc;
        version;
        name;
      } =
    let f = Option.map f in
    {
      deprecated = f deprecated;
      man_xrefs = f man_xrefs;
      man = f man;
      envs = f envs;
      exits = f exits;
      sdocs = f sdocs;
      docs = f docs;
      doc = f doc;
      version = f version;
      name = f name;
    }

  let tag_of_string = function
    | "deprecated" | "deprecated_" -> Some `deprecated
    | "man_xrefs" -> Some `man_xrefs
    | "man" -> Some `man
    | "envs" -> Some `envs
    | "exits" -> Some `exits
    | "sdocs" -> Some `sdocs
    | "docs" -> Some `docs
    | "doc" -> Some `doc
    | "version" -> Some `version
    | "name" -> Some `name
    | _ -> None

  let update_field_of_tag tag v t =
    match tag with
    | `deprecated -> { t with deprecated = Level.join t.deprecated v }
    | `man_xrefs -> { t with man_xrefs = Level.join t.man_xrefs v }
    | `man -> { t with man = Level.join t.man v }
    | `envs -> { t with envs = Level.join t.envs v }
    | `exits -> { t with exits = Level.join t.exits v }
    | `sdocs -> { t with sdocs = Level.join t.sdocs v }
    | `docs -> { t with docs = Level.join t.docs v }
    | `doc -> { t with doc = Level.join t.doc v }
    | `version -> { t with version = Level.join t.version v }
    | `name -> { t with name = Level.join t.name v }

  let parse attrs =
    parse_impl ~empty ~map ~tag_of_string ~update_field_of_tag attrs
end

module Default_term = struct
  let parse = parse_single "default"
end
