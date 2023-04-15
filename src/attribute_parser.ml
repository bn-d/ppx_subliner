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
    ~field_level_of_attr_name
    ~update_field
    (attrs : attributes) =
  attrs
  |> List.fold_left
       (fun acc attr ->
         let loc = attr.attr_loc in
         field_level_of_attr_name attr.attr_name
         |> function
         | None -> acc
         | Some field ->
             update_field ~loc (Level.get field)
               (Level.map
                  (fun _ ->
                    match attr.attr_payload with
                    | PStr structure -> (loc, structure)
                    | _ -> Error.attribute_payload ~loc)
                  field)
               acc)
       empty
  |> map Level.get

module Term = struct
  type 'a t = {
    (* info *)
    deprecated : 'a option;
    absent : 'a option;
    doc : 'a option;
    docs : 'a option;
    docv : 'a option;
    env : 'a option;
    (* named *)
    name : 'a option;
    (* positional *)
    pos : 'a option;
    pos_all : 'a option;
    pos_left : 'a option;
    pos_right : 'a option;
    (* list *)
    non_empty : 'a option;
    last : 'a option; (* TODO: support sep, t_sep, file, dir, non_dir_file, default *)
  }
  [@@deriving make]

  let empty = make_t ()

  let map
      f
      {
        deprecated;
        absent;
        doc;
        docs;
        docv;
        env;
        name;
        pos;
        pos_all;
        pos_left;
        pos_right;
        non_empty;
        last;
      } =
    let f = Option.map f in
    {
      deprecated = f deprecated;
      absent = f absent;
      doc = f doc;
      docs = f docs;
      docv = f docv;
      env = f env;
      (* named *)
      name = f name;
      (* positional *)
      pos = f pos;
      pos_all = f pos_all;
      pos_left = f pos_left;
      pos_right = f pos_right;
      (* list *)
      non_empty = f non_empty;
      last = f last;
    }

  let field_level_of_attr_name { txt = name; loc } =
    let valid =
      [
        "deprecated";
        "absent";
        "doc";
        "docs";
        "docv";
        "env";
        "name";
        "pos";
        "pos_all";
        "pos_left";
        "pos_right";
        "non_empty";
        "last";
      ]
    in
    match name with
    | "ocaml.doc" -> Some (Derived "doc")
    | "deprecated_" -> Some (General "deprecated")
    | "subliner.deprecated_" -> Some (Prefixed "deprecated")
    | _ when List.exists (( = ) name) valid -> Some (General name)
    | _ when Utils.string_starts_with ~prefix name ->
        let len = String.length name in
        let name = String.sub name prefix_len (len - prefix_len) in
        if List.exists (( = ) name) valid then
          Some (Prefixed name)
        else
          Error.attribute_name ~loc name
    | _ -> None

  let update_field ~loc name v t =
    match name with
    | "deprecated" -> { t with deprecated = Level.join t.deprecated v }
    | "absent" -> { t with absent = Level.join t.absent v }
    | "doc" -> { t with doc = Level.join t.doc v }
    | "docs" -> { t with docs = Level.join t.docs v }
    | "docv" -> { t with docv = Level.join t.docv v }
    | "env" -> { t with env = Level.join t.env v }
    | "name" -> { t with name = Level.join t.name v }
    | "pos" -> { t with pos = Level.join t.pos v }
    | "pos_all" -> { t with pos_all = Level.join t.pos_all v }
    | "pos_left" -> { t with pos_left = Level.join t.pos_left v }
    | "pos_right" -> { t with pos_right = Level.join t.pos_right v }
    | "non_empty" -> { t with non_empty = Level.join t.non_empty v }
    | "last" -> { t with last = Level.join t.last v }
    | name -> Error.attribute_name ~loc name

  (** parse attribute list to a static type *)
  let parse : attributes -> (location * structure) t =
    parse_impl ~empty ~map ~field_level_of_attr_name ~update_field
end

module Cmd_info = struct
  let to_args_label = function
    | "version" | "subliner.version" -> Some "version"
    | "deprecated" | "subliner.deprecated" | "deprecated_" -> Some "deprecated"
    | "docs" | "subliner.docs" -> Some "docs"
    | "sdocs" | "subliner.sdocs" -> Some "sdocs"
    | "exits" | "subliner.exits" -> Some "exits"
    | "envs" | "subliner.envs" -> Some "envs"
    | "man" | "subliner.man" -> Some "man"
    | "man_xrefs" | "subliner.man_xrefs" -> Some "man_xrefs"
    (* name and doc will be handled separately *)
    | "name" | "doc" | "ocaml.doc" | _ -> None

  let to_args ~(default_name_expr : expression) (attrs : attributes) :
      (arg_label * expression) list =
    (* get arguments that require special handling *)
    let name_arg =
      let expr =
        get_expr "name" attrs |> Option.value ~default:default_name_expr
      in
      [ (Nolabel, expr) ]
    and doc_arg =
      get_expr "doc" attrs
      |> (function Some e -> Some e | None -> get_expr "ocaml.doc" attrs)
      |> function Some e -> [ (Labelled "doc", e) ] | None -> []
    in
    List.filter_map
      (fun (attr : attribute) ->
        to_args_label attr.attr_name.txt
        |> Option.map (fun label ->
               let loc = attr.attr_loc in
               match attr.attr_payload with
               | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
                   (Labelled label, expr)
               | _ -> Error.attribute_payload ~loc))
      attrs
    @ doc_arg
    @ name_arg
end

module Default_term = struct
  let get : attributes -> expression option = get_expr "default"
end
