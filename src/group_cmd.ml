open Ppxlib

let gen_name_str = Printf.sprintf "make_%s_cmdliner_group_cmds"
let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

let term_expr_of_const_args ~loc (const_args : constructor_arguments) :
    expression =
  Ast_helper.with_default_loc loc (fun () ->
      match const_args with
      (* TODO: support enum *)
      | Pcstr_tuple [] -> failwith "not implemented"
      | Pcstr_tuple [ ct ] -> (
          match ct.ptyp_desc with
          | Ptyp_constr (lid, []) ->
              lid
              |> Utils.map_lid_name Term.gen_name_str
              |> Ast_helper.Exp.ident
          | _ -> Location.raise_errorf "constructor argument is not supported")
      | Pcstr_tuple _ ->
          Location.raise_errorf "constructor cannot have more than 1 arguments"
      | Pcstr_record _ ->
          Location.raise_errorf "inline record is currently not supported")

let cmd_vb_expr_of_const_decls
    (func_expr : expression)
    (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let name_str = cd.pcd_name.txt |> String.lowercase_ascii in
      let var_name = { txt = Printf.sprintf "subcmd_%s" name_str; loc } in

      let vb =
        let pat = Ast_helper.Pat.var var_name
        and expr =
          (* Cmd.info *)
          let cmd_info_expr =
            (* get cmd info from attributes *)
            let args =
              (* lower case constructor name will be the default cmd name *)
              let default_name_expr =
                Ast_builder.Default.estring ~loc:cd.pcd_name.loc name_str
              in
              Attribute_utils.Cmd_info.to_args ~default_name_expr
                cd.pcd_attributes
            in
            Ast_helper.Exp.apply [%expr Cmdliner.Cmd.info] args
            (* wrap params inside the current constructor *)
          and choice_expr =
            Ast_helper.Exp.construct
              (Utils.longident_loc_of_name cd.pcd_name)
              (Some [%expr params])
            (* 'a Term.t *)
          and term_expr = term_expr_of_const_args ~loc cd.pcd_args in

          [%expr
            let info : Cmdliner.Cmd.info = [%e cmd_info_expr]
            and f params = [%e func_expr] [%e choice_expr] in
            Cmdliner.(Cmd.v info Term.(const f $ [%e term_expr] ()))]
        in
        Ast_helper.Vb.mk pat expr
      and var_expr =
        var_name |> Utils.longident_loc_of_name |> Ast_helper.Exp.ident
      in
      (vb, var_expr))

let fun_core_type_of_type_name ~loc name =
  let lid = Utils.longident_loc_of_name name in
  let ct = Ast_helper.Typ.constr lid [] in
  [%type: ([%t ct] -> 'a) -> 'a Cmdliner.Cmd.t list]

let structure_of_const_decls ~loc name (cds : constructor_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let stri =
        let pat = Ast_helper.Pat.var @@ gen_name name
        and ct = fun_core_type_of_type_name ~loc name
        and func_expr = [%expr func] in
        let cmd_vbs, cmd_exprs =
          cds |> List.map (cmd_vb_expr_of_const_decls func_expr) |> List.split
        in

        let cmd_list_expr = Ast_builder.Default.elist ~loc cmd_exprs in
        let expr = Ast_helper.Exp.let_ Nonrecursive cmd_vbs cmd_list_expr in
        [%stri let ([%p pat] : [%t ct]) = fun func -> [%e expr]]
      in
      [ stri ])

let signature_of_const_decls ~loc name (_cds : constructor_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let sigi =
        let fun_name = gen_name name
        and ct = fun_core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ sigi ])
