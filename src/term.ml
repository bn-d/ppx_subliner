open Ppxlib
module Ap = Attribute_parser

let suffix = "cmdliner_term"
let gen_name_str = Utils.gen_name_str suffix
let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

type attrs = (location * structure) Ap.Term.t

module Conv = struct
  type t =
    | Bool
    | Char
    | Int
    | Nativeint
    | Int32
    | Int64
    | Float
    | String
    | File
    | Dir
    | Non_dir_file
    | Option of t
    | List of expression option * t
    | Array of expression option * t
    | Pair of expression option * (t * t)
    | T3 of expression option * (t * t * t)
    | T4 of expression option * (t * t * t * t)
    | Custom of expression

  let rec of_core_type ct =
    let conv = Ap.Conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "conv" in

    match ct with
    | _ when Option.is_some conv -> Custom (Option.get conv)
    | [%type: bool] | [%type: Bool.t] -> Bool
    | [%type: char] | [%type: Char.t] -> Char
    | [%type: int] | [%type: Int.t] -> Int
    | [%type: nativeint] | [%type: Nativeint.t] -> Nativeint
    | [%type: int32] | [%type: Int32.t] -> Int32
    | [%type: int64] | [%type: Int64.t] -> Int64
    | [%type: float] | [%type: Float.t] -> Float
    | ([%type: string] | [%type: String.t]) as ct ->
        let attrs = Ap.String_conv.parse ct.ptyp_attributes in
        let file = Ap.to_bool attrs.file
        and dir = Ap.to_bool attrs.dir
        and non_dir_file = Ap.to_bool attrs.non_dir_file in
        if file && not (dir || non_dir_file) then
          File
        else if dir && not (file || non_dir_file) then
          Dir
        else if non_dir_file && not (file || dir) then
          Non_dir_file
        else if not (file || dir || non_dir_file) then
          String
        else
          Location.raise_errorf ~loc:ct.ptyp_loc
            "only one of `dir`, `file` and `non_dir_file` can be specified at \
             the same time"
    | [%type: [%t? in_ct] option] | [%type: [%t? in_ct] Option.t] ->
        Option (of_core_type in_ct)
    | ([%type: [%t? in_ct] list] as ct) | ([%type: [%t? in_ct] List.t] as ct) ->
        let sep =
          Ap.Sep_conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "sep"
        in
        List (sep, of_core_type in_ct)
    | ([%type: [%t? in_ct] array] | [%type: [%t? in_ct] Array.t]) as ct ->
        let sep =
          Ap.Sep_conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "sep"
        in
        Array (sep, of_core_type in_ct)
    | [%type: [%t? t0] * [%t? t1]] as ct ->
        let sep =
          Ap.Sep_conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "sep"
        in
        Pair (sep, (of_core_type t0, of_core_type t1))
    | [%type: [%t? t0] * [%t? t1] * [%t? t2]] as ct ->
        let sep =
          Ap.Sep_conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "sep"
        in
        T3 (sep, (of_core_type t0, of_core_type t1, of_core_type t2))
    | [%type: [%t? t0] * [%t? t1] * [%t? t2] * [%t? t3]] as ct ->
        let sep =
          Ap.Sep_conv.parse ct.ptyp_attributes |> Ap.to_expr_opt "sep"
        in
        T4
          ( sep,
            (of_core_type t0, of_core_type t1, of_core_type t2, of_core_type t3)
          )
    | { ptyp_loc = loc; _ } -> Error.field_type ~loc

  let to_expr ~loc t : expression =
    let rec impl ~loc = function
      | Bool -> [%expr bool]
      | Char -> [%expr char]
      | Int -> [%expr int]
      | Nativeint -> [%expr nativeint]
      | Int32 -> [%expr int32]
      | Int64 -> [%expr int64]
      | Float -> [%expr float]
      | String -> [%expr string]
      | File -> [%expr file]
      | Dir -> [%expr dir]
      | Non_dir_file -> [%expr non_dir_file]
      | Option t ->
          let expr = impl ~loc t in
          [%expr some [%e expr]]
      | List (sep, t) ->
          let sep_expr =
            Option.fold ~none:[%expr None] ~some:(Utils.esome ~loc) sep
          and expr = impl ~loc t in
          [%expr list ?sep:[%e sep_expr] [%e expr]]
      | Array (sep, t) ->
          let sep_expr =
            Option.fold ~none:[%expr None] ~some:(Utils.esome ~loc) sep
          and expr = impl ~loc t in
          [%expr array ?sep:[%e sep_expr] [%e expr]]
      | Pair (sep, (t0, t1)) ->
          let sep_expr =
            Option.fold ~none:[%expr None] ~some:(Utils.esome ~loc) sep
          and t0_expr = impl ~loc t0
          and t1_expr = impl ~loc t1 in
          [%expr pair ?sep:[%e sep_expr] [%e t0_expr] [%e t1_expr]]
      | T3 (sep, (t0, t1, t2)) ->
          let sep_expr =
            Option.fold ~none:[%expr None] ~some:(Utils.esome ~loc) sep
          and t0_expr = impl ~loc t0
          and t1_expr = impl ~loc t1
          and t2_expr = impl ~loc t2 in
          [%expr t3 ?sep:[%e sep_expr] [%e t0_expr] [%e t1_expr] [%e t2_expr]]
      | T4 (sep, (t0, t1, t2, t3)) ->
          let sep_expr =
            Option.fold ~none:[%expr None] ~some:(Utils.esome ~loc) sep
          and t0_expr = impl ~loc t0
          and t1_expr = impl ~loc t1
          and t2_expr = impl ~loc t2
          and t3_expr = impl ~loc t3 in
          [%expr
            t4 ?sep:[%e sep_expr] [%e t0_expr] [%e t1_expr] [%e t2_expr]
              [%e t3_expr]]
      | Custom expr -> expr
    in

    let expr = impl ~loc t in
    [%expr Cmdliner.Arg.([%e expr])]
end

type conv = Conv.t

module Cmd_env_info = struct
  let expr_of_attrs ~loc (attrs : attrs) : expression option =
    Ap.to_expr_opt "env" attrs.env
    |> Option.map (fun env_epxr ->
           let args =
             let labelled =
               [
                 ( "deprecated",
                   Ap.to_expr_opt "env.deprecated" attrs.env_deprecated );
                 ("docs", Ap.to_expr_opt "env.docs" attrs.env_docs);
                 ("doc", Ap.to_expr_opt "env.doc" attrs.env_doc);
               ]
               |> List.filter_map (fun (name, expr_opt) ->
                      Option.map (fun expr -> (Labelled name, expr)) expr_opt)
             and no_label = [ (Nolabel, env_epxr) ] in
             labelled @ no_label
           in

           Ast_helper.Exp.apply ~loc [%expr Cmdliner.Cmd.Env.info] args)
end

module Info = struct
  let expr_of_attrs ~loc (names_expr : expression) (attrs : attrs) : expression
      =
    let args =
      let labelled =
        [
          ("deprecated", Ap.to_expr_opt "deprecated" attrs.deprecated);
          ("absent", Ap.to_expr_opt "absent" attrs.absent);
          ("docs", Ap.to_expr_opt "docs" attrs.docs);
          ("docv", Ap.to_expr_opt "docv" attrs.docv);
          ("doc", Ap.to_expr_opt "doc" attrs.doc);
          ("env", Cmd_env_info.expr_of_attrs ~loc attrs);
        ]
        |> List.filter_map (fun (name, expr_opt) ->
               Option.map (fun expr -> (Labelled name, expr)) expr_opt)
      (* names_expr should always resolved by Named or Positional *)
      and no_label = [ (Nolabel, names_expr) ] in
      labelled @ no_label
    in
    Ast_helper.Exp.apply ~loc [%expr Cmdliner.Arg.info] args
end

module As_term = struct
  let of_attrs ~loc (attrs : attrs) :
      [ `value of expression option
      | `non_empty
      | `last of expression option * expression option ] =
    let non_empty = Ap.to_bool attrs.non_empty
    and last = Ap.to_bool attrs.last
    and default = Ap.to_expr_opt "default" attrs.default in
    match (non_empty, last, default) with
    | true, false, None -> `non_empty
    | true, true, _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `last` cannot be used at the same time"
    | true, _, Some _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `default` cannot be used at the same time"
    | false, true, _ ->
        let sep = Ap.to_expr_opt "sep" attrs.last_sep in
        `last (sep, default)
    | false, false, _ -> `value default

  let to_expr ~loc :
      [< `value of 'a | `required | `non_empty | `last of 'b ] -> expression =
    function
    | `value _ -> [%expr Cmdliner.Arg.value]
    | `required -> [%expr Cmdliner.Arg.required]
    | `non_empty -> [%expr Cmdliner.Arg.non_empty]
    | `last _ -> [%expr Cmdliner.Arg.last]
end

module Named = struct
  let to_named_fun_expr ~loc = function
    | `flag -> [%expr Cmdliner.Arg.flag]
    | `flag_all -> [%expr Cmdliner.Arg.flag_all]
    | `opt (conv, default_expr) ->
        let conv_expr = Conv.to_expr ~loc conv in
        [%expr Cmdliner.Arg.opt [%e conv_expr] [%e default_expr]]
    | `opt_all (conv, default_expr) ->
        let conv_expr = Conv.to_expr ~loc conv in
        [%expr Cmdliner.Arg.opt_all [%e conv_expr] [%e default_expr]]

  let expr_of_attrs ~loc name ct (attrs : attrs) : expression =
    let as_term, type_ =
      let as_term = As_term.of_attrs ~loc attrs
      and conv = Conv.of_core_type ct
      and opt_all = Ap.to_bool attrs.opt_all in
      if not opt_all then
        match (as_term, conv) with
        | `value None, Bool -> (`value (), `flag)
        | `value (Some default_expr), _ -> (`value (), `opt (conv, default_expr))
        | `value None, Option _ -> (`value (), `opt (conv, [%expr None]))
        | `value None, _ -> (`required, `opt (Conv.Option conv, [%expr None]))
        | `non_empty, List _ -> (`non_empty, `opt (conv, [%expr []]))
        | `last (sep, default), _ ->
            let default_expr =
              Option.fold ~none:[%expr []] ~some:(Utils.elist ~loc) default
            in
            (`last (), `opt (Conv.List (sep, conv), default_expr))
        | `non_empty, _ -> Error.attr_list_type ~loc "non_empty"
      else
        match (as_term, conv) with
        | `value default, List (None, in_conv) ->
            let default_expr = Option.value ~default:[%expr []] default in
            (`value (), `opt_all (in_conv, default_expr))
        | `non_empty, List (None, in_conv) ->
            (`non_empty, `opt_all (in_conv, [%expr []]))
        | `last (_, default), _ ->
            let default_expr =
              Option.fold ~none:[%expr []] ~some:(Utils.elist ~loc) default
            in
            (`last (), `opt_all (conv, default_expr))
        | _, List (Some _, _) ->
            Location.raise_errorf ~loc
              "`opt_all` and `sep` cannot be used on the same list"
        | _ -> Error.attr_list_type ~loc "opt_all"
    and names_expr =
      (* field name will be the default arg name *)
      let default_names_expr =
        name.txt
        |> String.map (function '_' -> '-' | c -> c)
        |> Ast_builder.Default.estring ~loc:name.loc
        |> Utils.elist ~loc
      in
      attrs.names
      |> Ap.to_expr_opt "names"
      |> Option.value ~default:default_names_expr
    in

    let as_term_expr = As_term.to_expr ~loc as_term
    and named_fun_expr = to_named_fun_expr ~loc type_
    and info_expr = Info.expr_of_attrs ~loc names_expr attrs in

    [%expr [%e as_term_expr] ([%e named_fun_expr] [%e info_expr])]
end

module Positional = struct
  let pos_fun_expr_impl ~loc rev pos_expr fun_expr =
    let rev_expr = Ast_builder.Default.ebool ~loc rev in
    [%expr [%e fun_expr] ~rev:[%e rev_expr] [%e pos_expr]]

  let to_pos_fun_expr ~loc = function
    | `pos_all -> [%expr Cmdliner.Arg.pos_all]
    | `pos (r, p) -> pos_fun_expr_impl ~loc r p [%expr Cmdliner.Arg.pos]
    | `pos_left (r, p) ->
        pos_fun_expr_impl ~loc r p [%expr Cmdliner.Arg.pos_left]
    | `pos_right (r, p) ->
        pos_fun_expr_impl ~loc r p [%expr Cmdliner.Arg.pos_right]

  let expr_of_attrs ~loc ct (attrs : attrs) : expression =
    let () =
      attrs.names
      |> Ap.to_expr_opt "names"
      |> Option.fold ~none:() ~some:(fun _ ->
             Error.f ~loc "`names` cannot be used with positional argument")
    and () =
      if Ap.to_bool attrs.opt_all then
        Error.f ~loc "`opt_all` cannot be used with positional argument"
      else
        ()
    in
    let type_ =
      let rev = Ap.to_bool attrs.rev in
      match attrs with
      | { pos = Some pos; _ } ->
          let pos_expr = Ap.to_expr "pos" pos in
          `pos (rev, pos_expr)
      | { pos_left = Some pos; _ } ->
          let pos_expr = Ap.to_expr "pos_left" pos in
          `pos_left (rev, pos_expr)
      | { pos_right = Some pos; _ } ->
          let pos_expr = Ap.to_expr "pos_right" pos in
          `pos_right (rev, pos_expr)
      | { pos_all = Some _; _ } when rev ->
          Location.raise_errorf ~loc "`rev` cannot be used with `pos_all`"
      | { pos_all = Some pos; _ } ->
          let _ = Ap.to_bool (Some pos) in
          `pos_all
      | _ -> Error.unexpected ~loc
    in

    let as_term, conv, default_expr =
      let as_term = As_term.of_attrs ~loc attrs
      and conv = Conv.of_core_type ct in
      match type_ with
      | `pos _ -> (
          match (as_term, conv) with
          | `value (Some default_expr), _ -> (`value (), conv, default_expr)
          | `value None, Option _ -> (`value (), conv, [%expr None])
          | `value None, _ -> (`required, Option conv, [%expr None])
          | `non_empty, List _ -> (`non_empty, conv, [%expr []])
          | `last (sep, default), _ ->
              let default_expr =
                Option.fold ~none:[%expr []] ~some:(Utils.elist ~loc) default
              in
              (`last (), List (sep, conv), default_expr)
          | `non_empty, _ -> Error.attr_list_type ~loc "non_empty")
      | `pos_left _ | `pos_right _ | `pos_all -> (
          match (as_term, conv) with
          | `value default, List (None, in_conv) ->
              let default_expr = Option.value ~default:[%expr []] default in
              (`value (), in_conv, default_expr)
          | `non_empty, List (None, in_conv) -> (`non_empty, in_conv, [%expr []])
          | `last (_, default), _ ->
              let default_expr =
                Option.fold ~none:[%expr []] ~some:(Utils.elist ~loc) default
              in
              (`last (), conv, default_expr)
          | _, List (Some _, _) ->
              Location.raise_errorf ~loc
                "`sep` cannot be used with `pos_left`, `pos_right` and \
                 `pos_all`"
          | _ ->
              Location.raise_errorf ~loc
                "`pos_left`, `pos_right` and `pos_all` must be used with list \
                 type")
    in

    let as_term_expr = As_term.to_expr ~loc as_term
    and info_expr = Info.expr_of_attrs ~loc [%expr []] attrs
    and conv_expr = Conv.to_expr ~loc conv
    and pos_fun_expr = to_pos_fun_expr ~loc type_ in

    [%expr
      [%e as_term_expr]
        ([%e pos_fun_expr] [%e conv_expr] [%e default_expr] [%e info_expr])]
end

module T = struct
  let expr_of_attrs ~loc name ct (attrs : attrs) =
    let term = Ap.to_expr_opt "term" attrs.term in
    match term with
    | Some term_expr -> term_expr
    | None -> (
        let pos_count =
          let count opt = if Option.is_some opt then 1 else 0 in
          count attrs.pos
          + count attrs.pos_all
          + count attrs.pos_left
          + count attrs.pos_right
        in
        match pos_count with
        (* named *)
        | 0 -> Named.expr_of_attrs ~loc name ct attrs
        (* positional *)
        | 1 -> Positional.expr_of_attrs ~loc ct attrs
        (* multiple pos error *)
        | _ ->
            Location.raise_errorf ~loc
              "only one of `pos`, `pos_all`, `pos_left` and `pos_right` can be \
               specified at the same time")
end

let make_fun_vb_expr_of_label_decls ~loc ~const (lds : label_declaration list) =
  let vb =
    let pat = Ast_helper.Pat.var ~loc { txt = "make"; loc }
    and expr =
      lds
      |> List.map (fun ld ->
             let li = Utils.longident_loc_of_name ld.pld_name in
             (li, Ast_helper.Exp.ident ~loc li))
      |> fun fields ->
      Ast_helper.Exp.record ~loc fields None
      |> fun record_expr ->
      (match const with
      | None -> record_expr
      | Some const ->
          Ast_helper.Exp.construct ~loc
            (Utils.longident_loc_of_name const)
            (Some record_expr))
      |> fun value_expr ->
      List.fold_left
        (fun acc ld ->
          let pat = Ast_helper.Pat.var ~loc ld.pld_name in
          Ast_helper.Exp.fun_ ~loc Nolabel None pat acc)
        value_expr (List.rev lds)
    in
    Ast_helper.Vb.mk ~loc pat expr
  and var_expr = [%expr make] in
  (vb, var_expr)

let term_vb_expr_of_label_decl (ld : label_declaration) =
  let loc = ld.pld_loc in
  let name_str = ld.pld_name.txt in
  let var_name = { txt = Printf.sprintf "subterm_%s" name_str; loc } in

  let vb =
    let pat = Ast_helper.Pat.var ~loc var_name
    and expr =
      ld.pld_attributes
      |> Ap.Term.parse
      |> T.expr_of_attrs ~loc ld.pld_name ld.pld_type
    in
    Ast_helper.Vb.mk ~loc pat expr
  and var_expr =
    var_name |> Utils.longident_loc_of_name |> Ast_helper.Exp.ident ~loc
  in
  (vb, var_expr)

let aggregation_expr_of_term_exprs
    ~loc
    (make_expr : expression)
    (term_exprs : expression list) =
  let expr =
    List.fold_left
      (fun acc term_expr -> [%expr [%e acc] $ [%e term_expr]])
      [%expr const [%e make_expr]]
      term_exprs
  in
  [%expr Cmdliner.Term.([%e expr])]

let core_type_of_type_name ~loc name =
  let ct =
    let lid = Utils.longident_loc_of_name name in
    Ast_helper.Typ.constr lid []
  in
  [%type: unit -> [%t ct] Cmdliner.Term.t]

let expression_of_label_decls ~loc ~const (lds : label_declaration list) =
  let make_vb, make_expr = make_fun_vb_expr_of_label_decls ~loc ~const lds
  and term_vbs, term_exprs =
    lds |> List.map term_vb_expr_of_label_decl |> List.split
  in
  let aggregation_expr =
    aggregation_expr_of_term_exprs ~loc make_expr term_exprs
  in
  Ast_helper.Exp.let_ ~loc Nonrecursive (make_vb :: term_vbs) aggregation_expr

let structure_of_label_decls ~loc name (lds : label_declaration list) =
  let stri =
    let pat = Ast_helper.Pat.var ~loc @@ gen_name name
    and ct = core_type_of_type_name ~loc name
    and expr = expression_of_label_decls ~loc ~const:None lds in
    [%stri let ([%p pat] : [%t ct]) = fun () -> [%e expr]]
  in
  [ stri ]

let signature_of_label_decls ~loc name =
  let sigi =
    let fun_name = gen_name name and ct = core_type_of_type_name ~loc name in
    Ast_helper.Val.mk ~loc fun_name ct |> Ast_helper.Sig.value ~loc
  in
  [ sigi ]
