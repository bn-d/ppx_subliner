open Ppxlib

let gen_gcmds_fun_name { txt = name; loc } =
  { txt = Printf.sprintf "make_%s_cmdliner_group_cmds" name; loc }

(*let gen_eval_fun_name { txt = name; loc } =
  { txt = Printf.sprintf "%s_cmdliner_eval" name; loc }*)

let gen_term_name_str name = Printf.sprintf "%s_cmdliner_term" name

let map_lid_name f { txt; loc } =
  let rec impl = function
    | Lident str -> Lident (f str)
    | Ldot (t, str) -> Ldot (impl t, str)
    | _ -> Location.raise_errorf ~loc "Lapply of Longident is not supported"
  in
  { txt = impl txt; loc }

let cmd_vb_expr_of_variant_choice
    (func_expr : expression)
    (cd : constructor_declaration) =
  let loc = cd.pcd_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let name_str = cd.pcd_name.txt |> String.lowercase_ascii in
      (* lower case constructor name will be the default cmd name *)
      let default_name_expr =
        Ast_builder.Default.estring ~loc:cd.pcd_name.loc name_str
      in
      (* get cmd info from attributes *)
      let args =
        Attribute_utils.Cmd_info.to_args ~default_name_expr cd.pcd_attributes
      (* get Term.t *)
      and term_expr =
        match cd.pcd_args with
        (* TODO: support enum *)
        | Pcstr_tuple [] -> failwith "not implemented"
        | Pcstr_tuple [ ct ] -> (
            match ct.ptyp_desc with
            | Ptyp_constr (lid, []) ->
                lid |> map_lid_name gen_term_name_str |> Ast_helper.Exp.ident
            | _ -> Location.raise_errorf "constructor argument is not supported"
            )
        | Pcstr_tuple _ ->
            Location.raise_errorf
              "constructor cannot have more than 1 arguments"
        | Pcstr_record _ ->
            Location.raise_errorf "inline record is currently not supported"
      in
      let var_name = { txt = Printf.sprintf "subcmd_%s" name_str; loc }
      and cmd_info_expr = Ast_helper.Exp.apply [%expr Cmdliner.Cmd.info] args
      and choice_expr =
        Ast_helper.Exp.construct
          (Utils.longident_loc_of_name cd.pcd_name)
          (Some [%expr params])
      in
      let pat = Ast_helper.Pat.var var_name
      and expr =
        [%expr
          let info = [%e cmd_info_expr]
          and f params = [%e func_expr] [%e choice_expr] in
          Cmdliner.(Cmd.v info Term.(const f $ [%e term_expr] ()))]
      in

      let vb = Ast_helper.Vb.mk pat expr
      and ident_expr =
        var_name |> Utils.longident_loc_of_name |> Ast_helper.Exp.ident
      in
      (vb, ident_expr))

let fun_core_type_of_type_name ~loc name =
  let lid = Utils.longident_loc_of_name name in
  let type_ct = Ast_helper.Typ.constr lid [] in
  [%type: ([%t type_ct] -> 'a) -> 'a Cmdliner.Cmd.t list]

let structure_of_variant_choices
    ~loc
    name
    (cds : constructor_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let gcmds_fun =
        let pat = Ast_helper.Pat.var @@ gen_gcmds_fun_name name
        and ct = fun_core_type_of_type_name ~loc name
        and func_expr = [%expr func] in
        let cmd_vbs, cmd_exprs =
          cds
          |> List.map (cmd_vb_expr_of_variant_choice func_expr)
          |> List.split
        in

        let cmd_list_expr = Ast_builder.Default.elist ~loc cmd_exprs in
        let gcmds_expr =
          Ast_helper.Exp.let_ Nonrecursive cmd_vbs cmd_list_expr
        in
        [%stri let ([%p pat] : [%t ct]) = fun func -> [%e gcmds_expr]]
      in
      [ gcmds_fun ])

let signature_of_variant_choices ~loc name (_cds : constructor_declaration list)
    =
  Ast_helper.with_default_loc loc (fun () ->
      let gcmds_fun =
        let fun_name = gen_gcmds_fun_name name
        and ct = fun_core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ gcmds_fun ])
