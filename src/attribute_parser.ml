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

let get_expr name (attrs : attributes) =
  attrs
  |> Utils.list_find_map (fun (attr : attribute) ->
         if attr.attr_name.txt = name || attr.attr_name.txt = prefix ^ name then
           let loc = attr.attr_loc in
           match attr.attr_payload with
           | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Some expr
           | _ -> Error.attribute_payload ~loc
         else
           None)

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
                    | _ -> Error.attribute_payload ~loc)
                  field)
               acc)
       empty
  |> map Level.get

let to_bool =
  Option.fold ~none:false ~some:(function
    | _, [] -> true
    | loc, _ -> Error.attribute_flag ~loc)

let to_expr = function
  | _, [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> expr
  | loc, _ -> Error.attribute_payload ~loc

let to_expr_opt = Option.map to_expr

module Term = struct
  type 'a t = {
    (* info *)
    deprecated : 'a option;
    absent : 'a option;
    docs : 'a option;
    docv : 'a option;
    doc : 'a option;
    env : 'a option;
    (* named *)
    names : 'a option;
    opt_all : 'a option;
    (* positional *)
    pos : 'a option;
    pos_all : 'a option;
    pos_left : 'a option;
    pos_right : 'a option;
    (* list *)
    non_empty : 'a option;
    last : 'a option;
    (* misc *)
    default : 'a option;
        (* TODO: support list_sep, array_sep, tuple_sep, file, dir, non_dir_file, rev *)
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
        names;
        opt_all;
        pos;
        pos_all;
        pos_left;
        pos_right;
        non_empty;
        last;
        default;
      } =
    let f = Option.map f in
    {
      deprecated = f deprecated;
      absent = f absent;
      docs = f docs;
      docv = f docv;
      doc = f doc;
      env = f env;
      (* named *)
      names = f names;
      opt_all = f opt_all;
      (* positional *)
      pos = f pos;
      pos_all = f pos_all;
      pos_left = f pos_left;
      pos_right = f pos_right;
      (* list *)
      non_empty = f non_empty;
      last = f last;
      default = f default;
    }

  let tag_of_string = function
    | "deprecated" | "deprecated_" -> Some `deprecated
    | "absent" -> Some `absent
    | "docs" -> Some `docs
    | "docv" -> Some `docv
    | "doc" -> Some `doc
    | "env" -> Some `env
    | "names" -> Some `names
    | "opt_all" -> Some `opt_all
    | "pos" -> Some `pos
    | "pos_all" -> Some `pos_all
    | "pos_left" -> Some `pos_left
    | "pos_right" -> Some `pos_right
    | "non_empty" -> Some `non_empty
    | "last" -> Some `last
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
    | `names -> { t with names = Level.join t.names v }
    | `opt_all -> { t with opt_all = Level.join t.opt_all v }
    | `pos -> { t with pos = Level.join t.pos v }
    | `pos_all -> { t with pos_all = Level.join t.pos_all v }
    | `pos_left -> { t with pos_left = Level.join t.pos_left v }
    | `pos_right -> { t with pos_right = Level.join t.pos_right v }
    | `non_empty -> { t with non_empty = Level.join t.non_empty v }
    | `last -> { t with last = Level.join t.last v }
    | `default -> { t with default = Level.join t.default v }

  let parse attrs =
    parse_impl ~empty ~map ~tag_of_string ~update_field_of_tag attrs
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
  let get : attributes -> expression option = get_expr "default"
end