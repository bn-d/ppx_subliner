open Ppxlib

let suffix = "cmdliner_conv"
let gen_name_str = Utils.gen_name_str suffix
let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

let expression_of_const_decl (cd : constructor_declaration) : expression =
  let loc = cd.pcd_loc in
  let () =
    match cd.pcd_args with Pcstr_tuple [] -> () | _ -> Error.enum_payload ~loc
  in
  let enum_expr =
    Ast_helper.Exp.construct ~loc (Utils.longident_loc_of_name cd.pcd_name) None
  and names = Attribute_parser.Enum.parse cd.pcd_attributes in
  match names with
  | None ->
      let name_expr =
        let name =
          cd.pcd_name.txt
          |> String.lowercase_ascii
          |> String.map (function '_' -> '-' | c -> c)
        in
        Ast_builder.Default.estring ~loc name
      in
      [%expr [ ([%e name_expr], [%e enum_expr]) ]]
  | Some (loc, structure) ->
      let expr = Attribute_parser.to_expr "names" (loc, structure) in
      [%expr List.map (fun name -> (name, [%e enum_expr])) [%e expr]]

let core_type_of_type_name ~loc name =
  let ct =
    let lid = Utils.longident_loc_of_name name in
    Ast_helper.Typ.constr ~loc lid []
  in
  [%type: unit -> [%t ct] Cmdliner.Arg.conv]

let structure_of_const_decls ~loc name (cds : constructor_declaration list) =
  let stri =
    let pat = Ast_helper.Pat.var ~loc @@ gen_name name
    and ct = core_type_of_type_name ~loc name
    and expr =
      let enum_exprs = List.map expression_of_const_decl cds in
      Ast_builder.Default.elist ~loc enum_exprs
    in
    [%stri
      let ([%p pat] : [%t ct]) =
       fun () ->
        let enums = List.concat [%e expr] in
        Cmdliner.Arg.enum enums]
  in
  [ stri ]

let signature_of_const_decls ~loc name =
  let sigi =
    let fun_name = gen_name name and ct = core_type_of_type_name ~loc name in
    Ast_helper.Val.mk ~loc fun_name ct |> Ast_helper.Sig.value
  in
  [ sigi ]
