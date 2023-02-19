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
  let is_relevant = function
    (* TODO: deprecated_ support *)
    | "version" | "deprecated" | "docs" | "sdocs" | "exits" | "envs" | "man"
    | "man_xrefs" ->
        true
    (* name and doc will be handled separately *)
    | "name" | "doc" | "ocaml.doc" | _ -> false

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
        if is_relevant attr.attr_name.txt then
          let loc = attr.attr_loc in
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
              Some (Labelled attr.attr_name.txt, expr)
          | _ -> payload_error ~loc
        else
          None)
      attrs
    @ doc_arg
    @ name_arg
end
