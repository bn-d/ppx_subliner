open Ppxlib
module Ap = Attribute_parser

let suffix = "cmdliner_group_cmds"

let gen_name_str = function
  | "t" -> suffix
  | s -> Printf.sprintf "%s_%s" s suffix

let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

type info_attrs = expression Ap.Cmd_info.t

module Info = struct
  let expr_of_attrs ~loc (default_name_expr : expression) (attrs : attributes) :
      expression =
    Ast_helper.with_default_loc loc (fun () ->
        (* get cmd info args from attributes *)
        let info_attrs =
          Ap.Cmd_info.parse attrs
          |> Ap.Cmd_info.map (Ap.to_expr "Cmdliner.Cmd.info attributes")
        in
        let name_expr =
          Option.fold ~none:default_name_expr ~some:Fun.id info_attrs.name
        in
        let args =
          let labelled =
            [
              ("deprecated", info_attrs.deprecated);
              ("man_xrefs", info_attrs.man_xrefs);
              ("man", info_attrs.man);
              ("envs", info_attrs.envs);
              ("exits", info_attrs.exits);
              ("sdocs", info_attrs.sdocs);
              ("docs", info_attrs.docs);
              ("doc", info_attrs.doc);
              ("version", info_attrs.version);
            ]
            |> List.filter_map (fun (name, expr_opt) ->
                   Option.map (fun expr -> (Labelled name, expr)) expr_opt)
          and no_label = [ (Nolabel, name_expr) ] in
          labelled @ no_label
        in
        Ast_helper.Exp.apply [%expr Cmdliner.Cmd.info] args)
end

let handle_params_term_expr_of_const_decl
    ~loc
    func_expr
    (cd : constructor_declaration) : expression * expression =
  Ast_helper.with_default_loc loc (fun () ->
      match cd.pcd_args with
      | Pcstr_tuple [] ->
          let handle_expr =
            let choice_expr =
              Ast_helper.Exp.construct
                (Utils.longident_loc_of_name cd.pcd_name)
                None
            in
            [%expr fun () -> [%e func_expr] [%e choice_expr]]
          in
          (handle_expr, [%expr Cmdliner.Term.const ()])
      | Pcstr_tuple [ ct ] ->
          let handle_expr =
            let choice_expr =
              Ast_helper.Exp.construct
                (Utils.longident_loc_of_name cd.pcd_name)
                (Some [%expr params])
            in
            [%expr fun params -> [%e func_expr] [%e choice_expr]]
          and param_term_expr =
            let param_term_fun_expr =
              match ct.ptyp_desc with
              | Ptyp_constr (lid, []) ->
                  lid
                  |> Utils.map_lid_name Term.gen_name_str
                  |> Ast_helper.Exp.ident
              | _ ->
                  Location.raise_errorf "constructor argument is not supported"
            in
            [%expr [%e param_term_fun_expr] ()]
          in
          (handle_expr, param_term_expr)
      (* TODO: support multi arg and inline record *)
      | Pcstr_tuple _ ->
          Location.raise_errorf "constructor cannot have more than 1 arguments"
      | Pcstr_record lds ->
          let handle_expr = [%expr fun params -> [%e func_expr] params]
          and param_term_expr =
            Term.expression_of_label_decls ~loc ~const:(Some cd.pcd_name) lds
          in
          (handle_expr, param_term_expr))

let cmd_vb_expr_of_const_decl
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
            (* lower case constructor name will be the default cmd name *)
            let default_name_expr =
              name_str
              |> String.map (function '_' -> '-' | c -> c)
              |> Ast_builder.Default.estring ~loc:cd.pcd_name.loc
            in
            Info.expr_of_attrs ~loc default_name_expr cd.pcd_attributes
            (* ('params -> 'result) * 'params Term.t *)
          and handle_expr, params_term_expr =
            handle_params_term_expr_of_const_decl ~loc func_expr cd
          in
          [%expr
            let info : Cmdliner.Cmd.info = [%e cmd_info_expr]
            and handle = [%e handle_expr]
            and params_term = [%e params_term_expr] in
            Cmdliner.(Cmd.v info Term.(const handle $ params_term))]
        in
        Ast_helper.Vb.mk pat expr
      and var_expr =
        var_name |> Utils.longident_loc_of_name |> Ast_helper.Exp.ident
      in
      (vb, var_expr))

let core_type_of_type_name ~loc name =
  Ast_helper.with_default_loc loc (fun () ->
      let ct =
        let lid = Utils.longident_loc_of_name name in
        Ast_helper.Typ.constr lid []
      in
      [%type: ([%t ct] -> 'a) -> 'a Cmdliner.Cmd.t list])

let structure_of_const_decls ~loc name (cds : constructor_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let stri =
        let pat = Ast_helper.Pat.var @@ gen_name name
        and ct = core_type_of_type_name ~loc name
        and expr =
          let cmd_vbs, cmd_exprs =
            cds
            |> List.map (cmd_vb_expr_of_const_decl [%expr func])
            |> List.split
          in
          let cmd_list_expr = Ast_builder.Default.elist ~loc cmd_exprs in
          Ast_helper.Exp.let_ Nonrecursive cmd_vbs cmd_list_expr
        in
        [%stri let ([%p pat] : [%t ct]) = fun func -> [%e expr]]
      in
      [ stri ])

let signature_of_const_decls ~loc name (_cds : constructor_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let sigi =
        let fun_name = gen_name name
        and ct = core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ sigi ])
