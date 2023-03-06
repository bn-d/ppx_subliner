open Ppxlib

let payload_error ~loc =
  Location.raise_errorf ~loc "payload of this attribute of is not supported"

let get_expr name (attrs : attributes) =
  attrs
  |> Utils.list_find_map (fun (attr : attribute) ->
         if attr.attr_name.txt = name then
           let loc = attr.attr_loc in
           match attr.attr_payload with
           | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Some expr
           | _ -> payload_error ~loc
         else
           None)

module Cmd_info = struct
  let to_args_label = function
    (* TODO: deprecated_ support *)
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
        get_expr "subliner.name" attrs
        |> (function Some e -> Some e | None -> get_expr "name" attrs)
        |> Option.value ~default:default_name_expr
      in
      [ (Nolabel, expr) ]
    and doc_arg =
      get_expr "subliner.doc" attrs
      |> (function Some e -> Some e | None -> get_expr "doc" attrs)
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
               | _ -> payload_error ~loc))
      attrs
    @ doc_arg
    @ name_arg
end

module Default_term = struct
  let get : attributes -> expression option =
    Utils.list_find_map (fun (attr : attribute) ->
        let loc = attr.attr_loc in
        if
          attr.attr_name.txt = "default"
          || attr.attr_name.txt = "subliner.default"
        then
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Some expr
          | _ -> payload_error ~loc
        else
          None)
end
