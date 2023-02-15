open Ppxlib

let payload_error ~loc =
  Location.raise_errorf ~loc "payload of this attribute of is not supported"

module Cmd_info = struct
  let is_relevant = function
    | "version" | "deprecated" | "doc" | "docs" | "sdocs" -> true
    (* name will be handled separately *)
    (* TODO: exits envs man man_xrefs *)
    | "name" | _ -> false

  let to_args ~(default_name_expr : expression) (attrs : attributes) :
      (arg_label * expression) list =
    (* get the positional argument first *)
    let name_arg =
      let expr =
        attrs
        |> List.find_map (fun (attr : attribute) ->
               if attr.attr_name.txt = "name" then
                 let loc = attr.attr_loc in
                 match attr.attr_payload with
                 | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> Some expr
                 | _ -> payload_error ~loc
               else
                 None)
        |> Option.value ~default:default_name_expr
      in
      (Nolabel, expr)
    in
    List.filter_map
      (fun (attr : attribute) ->
        if is_relevant attr.attr_name.txt then
          let loc = attr.attr_loc in
          match attr.attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
              Some (Labelled attr.attr_name.txt, expr)
              (* special handling for bool *)
          | PStr [] ->
              if attr.attr_name.txt = "deprecated" then
                Some (Labelled attr.attr_name.txt, [%expr true])
              else
                payload_error ~loc
          | _ -> payload_error ~loc
        else
          None)
      attrs
    @ [ name_arg ]
end
