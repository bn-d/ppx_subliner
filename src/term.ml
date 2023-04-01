open Ppxlib

let suffix = "cmdliner_term"

let gen_name_str = function
  | "t" -> suffix
  | s -> Printf.sprintf "%s_%s" s suffix

let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

module T = struct
  type positional_type =
    | Pos of int
    | Pos_all
    | Pos_left of int
    | Pos_right of int

  type arg_type = Optional | Required | Non_empty | Last

  type info = {
    deprecated_ : expression option;
    absent : expression option;
    doc : expression option;
    docs : expression option;
    docv : expression option;
    env : expression option;
  }

  type named_arg = { type_ : arg_type; name : string; info : info }
  type positional_arg = { type_ : positional_type; info : info }
  type t = Named of named_arg | Positional of positional_arg
  type term_attr = (location * structure) Attribute_utils.Term.t

  let info_of_term_attr (term_attr : term_attr) : info =
    let f = Option.map Utils.expression_of_structure in
    {
      deprecated_ = f term_attr.deprecated_;
      absent = f term_attr.absent;
      doc = f term_attr.doc;
      docs = f term_attr.docs;
      docv = f term_attr.docv;
      env = f term_attr.env;
    }

  let named_of_term_attr ~loc name ct (term_attr : term_attr) =
    let type_ = failwith ""
    and name = failwith ""
    and info = info_of_term_attr term_attr in
    { type_; name; info }

  let of_term_attr ~loc name ct (term_attr : term_attr) =
    let pos_count =
      let count opt = if Option.is_some opt then 1 else 0 in
      count term_attr.pos
      + count term_attr.pos_all
      + count term_attr.pos_left
      + count term_attr.pos_right
    in
    match pos_count with
    (* named *)
    | 0 -> Named (named_of_term_attr ~loc name ct term_attr)
    (* positional *)
    | 1 -> failwith ""
    (* multiple pos error *)
    | _ ->
        Location.raise_errorf ~loc
          "only one of [pos|pos_all|pos_left|pos_right] can be specified at \
           the same time"

  let to_expr = failwith ""
end

let make_fun_vb_expr_of_label_decls ~loc (lds : label_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let vb =
        let pat = Ast_helper.Pat.var { txt = "make"; loc }
        and expr =
          lds
          |> List.map (fun ld ->
                 let li = Utils.longident_loc_of_name ld.pld_name in
                 (li, Ast_helper.Exp.ident li))
          |> fun fields ->
          Ast_helper.Exp.record fields None
          |> fun record_expr ->
          List.fold_left
            (fun acc ld ->
              let pat = Ast_helper.Pat.var ld.pld_name in
              Ast_helper.Exp.fun_ Nolabel None pat acc)
            record_expr (List.rev lds)
        in
        Ast_helper.Vb.mk pat expr
      and var_expr = [%expr make] in
      (vb, var_expr))

let term_vb_expr_of_label_decl (ld : label_declaration) =
  let loc = ld.pld_loc in
  Ast_helper.with_default_loc loc (fun () ->
      let name_str = ld.pld_name.txt in
      let var_name = { txt = Printf.sprintf "subterm_%s" name_str; loc } in

      let vb =
        let pat = Ast_helper.Pat.var var_name
        and expr =
          ld.pld_attributes
          |> Attribute_utils.Term.parse
          |> T.of_term_attr
          |> T.to_expr
        in
        Ast_helper.Vb.mk pat expr
      and var_expr =
        var_name |> Utils.longident_loc_of_name |> Ast_helper.Exp.ident
      in
      (vb, var_expr))

let aggregation_expr_of_term_exprs
    ~loc
    (make_expr : expression)
    (term_exprs : expression list) =
  Ast_helper.with_default_loc loc (fun () ->
      let expr =
        List.fold_left
          (fun acc term_expr -> [%expr [%e acc] $ [%e term_expr]])
          [%expr const [%e make_expr]]
          term_exprs
      in
      [%expr Cmdliner.Term.([%e expr])])

let core_type_of_type_name ~loc name =
  let ct =
    let lid = Utils.longident_loc_of_name name in
    Ast_helper.Typ.constr lid []
  in
  [%type: unit -> [%t ct] Cmdliner.Term.t]

let structure_of_label_decls ~loc name (lds : label_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let stri =
        let pat = Ast_helper.Pat.var @@ gen_name name
        and ct = core_type_of_type_name ~loc name
        and expr =
          let make_vb, make_expr = make_fun_vb_expr_of_label_decls ~loc lds
          and term_vbs, term_exprs =
            lds |> List.map term_vb_expr_of_label_decl |> List.split
          in
          let aggregation_expr =
            aggregation_expr_of_term_exprs ~loc make_expr term_exprs
          in
          Ast_helper.Exp.let_ Nonrecursive (make_vb :: term_vbs)
            aggregation_expr
        in
        [%stri let ([%p pat] : [%t ct]) = fun () -> [%e expr]]
      in
      [ stri ])

let signature_of_lablel_decls ~loc name (_lds : label_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let sigi =
        let fun_name = gen_name name
        and ct = core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ sigi ])
