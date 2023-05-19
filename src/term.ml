open Ppxlib

let suffix = "cmdliner_term"

let gen_name_str = function
  | "t" -> suffix
  | s -> Printf.sprintf "%s_%s" s suffix

let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

type attrs = (location * structure) Attribute_parser.Term.t

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
    | List of t
    | Array of t
  (* TODO: support Pair | T3 | T4 *)

  let rec of_core_type : core_type -> t = function
    | [%type: bool] | [%type: Bool.t] -> Bool
    | [%type: char] | [%type: Char.t] -> Char
    | [%type: int] | [%type: Int.t] -> Int
    | [%type: nativeint] | [%type: Nativeint.t] -> Nativeint
    | [%type: int32] | [%type: Int32.t] -> Int32
    | [%type: int64] | [%type: Int64.t] -> Int64
    | [%type: float] | [%type: Float.t] -> Float
    | [%type: string] | [%type: String.t] -> String
    | [%type: [%t? ct] option] | [%type: [%t? ct] Option.t] ->
        Option (of_core_type ct)
    | [%type: [%t? ct] list] | [%type: [%t? ct] List.t] ->
        List (of_core_type ct)
    | [%type: [%t? ct] array] | [%type: [%t? ct] Array.t] ->
        Array (of_core_type ct)
    | { ptyp_loc = loc; _ } -> Error.field_type ~loc

  let to_expr ~loc (_attrs : attrs) t : expression =
    (* TODO: impl sep *)
    let list_sep_expr = None |> Option.value ~default:[%expr None]
    and array_sep_expr = None |> Option.value ~default:[%expr None] in
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
      | List t ->
          let expr = impl ~loc t in
          [%expr list ?sep:[%e list_sep_expr] [%e expr]]
      | Array t ->
          let expr = impl ~loc t in
          [%expr array ?sep:[%e array_sep_expr] [%e expr]]
    in

    let expr = impl ~loc t in
    [%expr Cmdliner.Arg.([%e expr])]
end

type conv = Conv.t

module Info = struct
  let expr_of_attrs ~loc (names_expr : expression) (attrs : attrs) : expression
      =
    Ast_helper.with_default_loc loc (fun () ->
        let args =
          let labelled =
            let f = Attribute_parser.to_expr_opt in
            [
              ("deprecated", f attrs.deprecated);
              ("absent", f attrs.absent);
              ("docs", f attrs.docs);
              ("docv", f attrs.docv);
              ("doc", f attrs.doc);
              ("env", f attrs.env);
            ]
            |> List.filter_map (fun (name, expr_opt) ->
                   Option.map (fun expr -> (Labelled name, expr)) expr_opt)
          (* names_expr should always resolved by Named or Positional *)
          and no_label = [ (Nolabel, names_expr) ] in
          labelled @ no_label
        in
        Ast_helper.Exp.apply [%expr Cmdliner.Arg.info] args)
end

module As_term = struct
  let of_attrs ~loc (attrs : attrs) :
      [ `value of expression option | `non_empty | `last of expression option ]
      =
    let non_empty = Attribute_parser.to_bool attrs.non_empty
    and last = Attribute_parser.to_bool attrs.last
    and default = Attribute_parser.to_expr_opt attrs.default in
    match (non_empty, last, default) with
    | true, false, None -> `non_empty
    | true, true, _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `last` cannot be used at the same time"
    | true, _, Some _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `default` cannot be used at the same time"
    | false, true, _ -> `last default
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
  let to_named_fun_expr ~loc attrs = function
    | `flag -> [%expr Cmdliner.Arg.flag]
    | `flag_all -> [%expr Cmdliner.Arg.flag_all]
    | `opt (conv, default_expr) ->
        let conv_expr = Conv.to_expr ~loc attrs conv in
        [%expr Cmdliner.Arg.opt [%e conv_expr] [%e default_expr]]
    | `opt_all (conv, default_expr) ->
        let conv_expr = Conv.to_expr ~loc attrs conv in
        [%expr Cmdliner.Arg.opt_all [%e conv_expr] [%e default_expr]]

  let expr_of_attrs ~loc name ct (attrs : attrs) : expression =
    let as_term, type_ =
      let as_term = As_term.of_attrs ~loc attrs
      and conv = Conv.of_core_type ct
      and opt_all = Attribute_parser.to_bool attrs.opt_all in
      if not opt_all then
        match (as_term, conv) with
        | `value None, Bool -> (`value (), `flag)
        | `value (Some default_expr), _ -> (`value (), `opt (conv, default_expr))
        | `value None, Option _ -> (`value (), `opt (conv, [%expr None]))
        | `value None, _ -> (`required, `opt (Conv.Option conv, [%expr None]))
        | `non_empty, List _ -> (`non_empty, `opt (conv, [%expr []]))
        | `last default, _ ->
            let default_expr =
              Option.fold ~none:[%expr []]
                ~some:(fun expr -> [%expr [ [%e expr] ]])
                default
            in
            (`last (), `opt (Conv.List conv, default_expr))
        | `non_empty, _ -> Error.non_empty_list ~loc
      else
        match (as_term, conv) with
        | `value default, List in_conv ->
            let default_expr = Option.value ~default:[%expr []] default in
            (`value (), `opt_all (in_conv, default_expr))
        | `non_empty, List in_conv ->
            (`non_empty, `opt_all (in_conv, [%expr []]))
        | `last default, _ ->
            let default_expr =
              Option.fold ~none:[%expr []]
                ~some:(fun expr -> [%expr [ [%e expr] ]])
                default
            in
            (`last (), `opt_all (conv, default_expr))
        | _ ->
            Location.raise_errorf ~loc "`opt_all` must be used with list type"
    and names_expr =
      (* field name will be the default arg name *)
      let default_names_expr =
        let default_name_expr =
          Ast_builder.Default.estring ~loc:name.loc name.txt
        in
        [%expr [ [%e default_name_expr] ]]
      in
      attrs.names
      |> Attribute_parser.to_expr_opt
      |> Option.value ~default:default_names_expr
    in

    let as_term_expr = As_term.to_expr ~loc as_term
    and named_fun_expr = to_named_fun_expr ~loc attrs type_
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
      |> Attribute_parser.to_expr_opt
      |> Option.fold ~none:() ~some:(fun _ ->
             Error.f ~loc "`names` cannot be used with positional argument")
    and () =
      if Attribute_parser.to_bool attrs.opt_all then
        Error.f ~loc "`opt_all` cannot be used with positional argument"
      else
        ()
    in
    let type_ =
      let rev = false (* TODO: support rev *) in
      match attrs with
      | { pos = Some pos; _ } ->
          let pos_expr = Attribute_parser.to_expr pos in
          `pos (rev, pos_expr)
      | { pos_left = Some pos; _ } ->
          let pos_expr = Attribute_parser.to_expr pos in
          `pos_left (rev, pos_expr)
      | { pos_right = Some pos; _ } ->
          let pos_expr = Attribute_parser.to_expr pos in
          `pos_right (rev, pos_expr)
      | { pos_all = Some _; _ } when rev ->
          Location.raise_errorf ~loc "`rev` cannot be used with `pos_all`"
      | { pos_all = Some pos; _ } ->
          let _ = Attribute_parser.to_bool (Some pos) in
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
          | `last default, _ ->
              let default_expr =
                Option.fold ~none:[%expr []]
                  ~some:(fun expr -> [%expr [ [%e expr] ]])
                  default
              in
              (`last (), List conv, default_expr)
          | `non_empty, _ -> Error.non_empty_list ~loc)
      | `pos_left _ | `pos_right _ | `pos_all -> (
          match (as_term, conv) with
          | `value default, List in_conv ->
              let default_expr = Option.value ~default:[%expr []] default in
              (`value (), in_conv, default_expr)
          | `non_empty, List in_conv -> (`non_empty, in_conv, [%expr []])
          | `last default, _ ->
              let default_expr =
                Option.fold ~none:[%expr []]
                  ~some:(fun expr -> [%expr [ [%e expr] ]])
                  default
              in
              (`last (), conv, default_expr)
          | _ ->
              Location.raise_errorf ~loc
                "`pos_left`, `pos_right` and `pos_all` must be used with list \
                 type")
    in

    let as_term_expr = As_term.to_expr ~loc as_term
    and info_expr = Info.expr_of_attrs ~loc [%expr []] attrs
    and conv_expr = Conv.to_expr ~loc attrs conv
    and pos_fun_expr = to_pos_fun_expr ~loc type_ in

    [%expr
      [%e as_term_expr]
        ([%e pos_fun_expr] [%e conv_expr] [%e default_expr] [%e info_expr])]
end

module T = struct
  let expr_of_attrs ~loc name ct (attrs : attrs) =
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
          "only one of [pos|pos_all|pos_left|pos_right] can be specified at \
           the same time"
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
          |> Attribute_parser.Term.parse
          |> T.expr_of_attrs ~loc ld.pld_name ld.pld_type
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

let signature_of_label_decls ~loc name (_lds : label_declaration list) =
  Ast_helper.with_default_loc loc (fun () ->
      let sigi =
        let fun_name = gen_name name
        and ct = core_type_of_type_name ~loc name in
        Ast_helper.Val.mk fun_name ct |> Ast_helper.Sig.value
      in
      [ sigi ])
