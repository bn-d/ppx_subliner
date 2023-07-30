open Ppxlib

let suffix = "cmdliner_conv"
let gen_name_str = Utils.gen_name_str suffix
let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

let core_type_of_type_name ~loc name =
  Ast_helper.with_default_loc loc (fun () ->
      let ct =
        let lid = Utils.longident_loc_of_name name in
        Ast_helper.Typ.constr lid []
      in
      [%type: [%t ct] Cmdliner.Arg.conv])

let structure_of_const_decls ~loc:_ _name _cds = failwith ""

let signature_of_const_decls ~loc name =
  Ast_helper.with_default_loc loc (fun () ->
      let sigi =
        let fun_name = gen_name name
        and ct = core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ sigi ])
