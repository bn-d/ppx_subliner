open Ppxlib

type eval_type = Cmds | Term

let eval_type_of_name = function
  | "subliner.cmds" -> Some Cmds
  | "subliner.term" -> Some Term
  | _ -> None

let unsupported_error ~loc =
  Location.raise_errorf ~loc
    "extension payload is not supported. please ensure it is in the format of \
     [%%subliner.[cmds|term] <eval>.<params type> <- <function expression>]"

let cmd_expr_of_func_expr ~loc ~attrs t lid func_expr : expression =
  match t with
  | Cmds ->
      (* Cmd.info *)
      let cmd_info_expr =
        (* exe name will be the default cmd name *)
        let default_name_expr = [%expr Filename.basename Sys.argv.(0)] in
        Group_cmds.Info.expr_of_attrs ~loc default_name_expr attrs
      and default_term_expr =
        Attribute_parser.Default_term.get attrs
        |> Option.value
             ~default:[%expr Cmdliner.Term.(ret (const (`Help (`Auto, None))))]
      and group_cmd_fun_expr =
        lid
        |> Utils.map_lid_name Group_cmds.gen_name_str
        |> Ast_helper.Exp.ident
      in
      [%expr
        let info : Cmdliner.Cmd.info = [%e cmd_info_expr]
        and default = [%e default_term_expr]
        and group_cmd = [%e group_cmd_fun_expr] [%e func_expr] in
        Cmdliner.Cmd.group ~default info group_cmd]
  | Term ->
      (* Cmd.info *)
      let cmd_info_expr =
        (* exe name will be the default cmd name *)
        let default_name_expr = [%expr Filename.basename Sys.argv.(0)] in
        Group_cmds.Info.expr_of_attrs ~loc default_name_expr attrs
      and params_term_expr =
        lid |> Utils.map_lid_name Term.gen_name_str |> Ast_helper.Exp.ident
      in
      [%expr
        let info : Cmdliner.Cmd.info = [%e cmd_info_expr] in
        Cmdliner.(
          Cmd.v info Term.(const [%e func_expr] $ [%e params_term_expr] ()))]

let eval_fun_of_expr ~loc ~attrs t (expr : expression) : structure_item =
  match expr.pexp_desc with
  | Pexp_setfield (eval_expr, type_lid, func_expr) ->
      Ast_helper.with_default_loc expr.pexp_loc (fun () ->
          let cmd_expr =
            cmd_expr_of_func_expr ~loc ~attrs t type_lid func_expr
          in
          [%stri
            let () =
              let cmd = [%e cmd_expr] in
              exit (Cmdliner.Cmd.([%e eval_expr]) cmd)])
  | _ -> unsupported_error ~loc

let eval_fun_of_payload ~loc ~attrs t : payload -> structure_item = function
  | PStr [ { pstr_desc = Pstr_eval (expr, _attrs); _ } ] ->
      eval_fun_of_expr ~loc ~attrs t expr
  | _ -> unsupported_error ~loc

let impl (strs : structure_item list) : structure_item list =
  (* TODO: take care module doc *)
  List.filter_map
    (fun str ->
      let loc = str.pstr_loc in
      match str.pstr_desc with
      | Pstr_extension (({ txt; loc = _ }, payload), attrs)
        when Utils.string_starts_with ~prefix:"subliner" txt -> (
          match eval_type_of_name txt with
          | Some t -> Some (eval_fun_of_payload ~loc ~attrs t payload)
          | None -> Location.raise_errorf "unknown subliner rewriter name")
      | _ -> Some str)
    strs

let () = Driver.register_transformation ~impl "subliner"
