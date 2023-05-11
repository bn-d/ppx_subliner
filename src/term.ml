open Ppxlib

let suffix = "cmdliner_term"

let gen_name_str = function
  | "t" -> suffix
  | s -> Printf.sprintf "%s_%s" s suffix

let gen_name { txt = name; loc } = { txt = gen_name_str name; loc }

module Conv = struct
  type basic =
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
  (* TODO: support Pair | T3 | T4 *)

  type complex =
    | Basic of basic
    | Option of basic
    | List of { sep_expr : expression option; basic : basic }
    | Array of { sep_expr : expression option; basic : basic }

  type t = Location.t * complex

  let basic_of_core_type : core_type -> basic = function
    | [%type: bool] | [%type: Bool.t] -> Bool
    | [%type: char] | [%type: Char.t] -> Char
    | [%type: int] | [%type: Int.t] -> Int
    | [%type: nativeint] | [%type: Nativeint.t] -> Nativeint
    | [%type: int32] | [%type: Int32.t] -> Int32
    | [%type: int64] | [%type: Int64.t] -> Int64
    | [%type: float] | [%type: Float.t] -> Float
    | [%type: string] | [%type: String.t] -> String
    | { ptyp_loc = loc; _ } -> Error.field_type ~loc

  let of_core_type (ct : core_type) : t =
    let loc = ct.ptyp_loc in
    match ct with
    | [%type: [%t? ct] option] | [%type: [%t? ct] Option.t] ->
        (loc, Option (basic_of_core_type ct))
    | [%type: [%t? ct] list] | [%type: [%t? ct] List.t] ->
        (loc, List { sep_expr = None; basic = basic_of_core_type ct })
    | [%type: [%t? ct] array] | [%type: [%t? ct] Array.t] ->
        (loc, Array { sep_expr = None; basic = basic_of_core_type ct })
    | { ptyp_desc = Ptyp_constr (_, []); _ } ->
        (loc, Basic (basic_of_core_type ct))
    (* TODO: add support for custom conv *)
    | _ -> Error.field_type ~loc:ct.ptyp_loc

  let basic_to_expr ~loc : basic -> expression = function
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

  let to_expr ((loc, complex) : t) : expression =
    Ast_helper.with_default_loc loc (fun () ->
        let complex_expr =
          match complex with
          | Basic basic -> basic_to_expr ~loc basic
          | Option basic ->
              let basic_expr = basic_to_expr ~loc basic in
              [%expr some [%e basic_expr]]
          | List { sep_expr; basic } ->
              let sep_expr = Option.value ~default:[%expr ','] sep_expr
              and basic_expr = basic_to_expr ~loc basic in
              [%expr list ~sep:[%e sep_expr] [%e basic_expr]]
          | Array { sep_expr; basic } ->
              let sep_expr = Option.value ~default:[%expr ','] sep_expr
              and basic_expr = basic_to_expr ~loc basic in
              [%expr array ~sep:[%e sep_expr] [%e basic_expr]]
        in
        [%expr Cmdliner.Arg.([%e complex_expr])])
end

type term_attr = (location * structure) Attribute_parser.Term.t

module Info = struct
  let expr_of_term_attr ~loc (names_expr : expression) (term_attr : term_attr) :
      expression =
    Ast_helper.with_default_loc loc (fun () ->
        let args =
          let labelled =
            let f = Option.map Utils.expression_of_structure in
            [
              ("deprecated", f term_attr.deprecated);
              ("absent", f term_attr.absent);
              ("docs", f term_attr.docs);
              ("docv", f term_attr.docv);
              ("doc", f term_attr.doc);
              ("env", f term_attr.env);
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
  (* of default_expression option *)
  type t = Value of expression option | Non_empty | Last of expression option

  let of_term_attr ~loc (term_attr : term_attr) : t =
    let f =
      Option.fold ~none:false ~some:(function
        | _, [] -> true
        | _ ->
            Location.raise_errorf ~loc
              "`non_empty` and `last` cannot have any payload")
    in
    let non_empty = f term_attr.non_empty
    and last = f term_attr.last
    and (* TODO: implement default *)
        default = None in
    match (non_empty, last, default) with
    | true, false, None -> Non_empty
    | true, true, _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `last` cannot be used at the same time"
    | true, _, Some _ ->
        Location.raise_errorf ~loc
          "`non_empty` and `default` cannot be used at the same time"
    | false, true, _ -> Last default
    | false, false, _ -> Value default
end

module Named = struct
  type category =
    | Flag
    | Flag_all
    | Opt of { default_expr : expression; conv : Conv.t }
    | Opt_all of { default_expr : expression; conv : Conv.t }
    | Required of Conv.t
    | Non_empty_opt of Conv.t
    | Non_empty_opt_all of Conv.t
    | Non_empty_flag_all
    | Last of { default_expr : expression; conv : Conv.t }

  type t = { category : category; info_expr : expression }

  let category_of_non_empty ~loc (conv : Conv.t) : category =
    match snd conv with
    | List { sep_expr = None; basic = Bool } -> Non_empty_flag_all
    | List { sep_expr = None; basic } ->
        Non_empty_opt_all (fst conv, Basic basic)
    | List { sep_expr = Some _; basic = _ } -> Non_empty_opt conv
    | _ -> Location.raise_errorf ~loc "`non_empty` can only be used with `list`"

  let category_of_value ~loc default (conv : Conv.t) : category =
    match (snd conv, default) with
    (* bool without any modifier will be flag *)
    | Basic Bool, None -> Flag
    | List { sep_expr = None; basic = Bool }, None -> Flag_all
    (* list without sep will be opt_all *)
    | List { sep_expr = None; basic }, default ->
        let default_expr = Option.value ~default:[%expr []] default in
        Opt_all { default_expr; conv = (fst conv, Basic basic) }
    (* opt *)
    | Option _, None -> Opt { default_expr = [%expr None]; conv }
    | Array _, None -> Opt { default_expr = [%expr [||]]; conv }
    | _, Some default_expr -> Opt { default_expr; conv }
    | _, None -> Required conv

  let of_term_attr ~loc name ct (term_attr : term_attr) : t =
    let category =
      let conv = Conv.of_core_type ct
      and as_term = As_term.of_term_attr ~loc term_attr in
      match as_term with
      | Non_empty -> category_of_non_empty ~loc conv
      | Last default ->
          let default_expr =
            Option.fold ~none:[%expr []]
              ~some:(fun default_expr -> [%expr [ [%e default_expr] ]])
              default
          in
          Last { default_expr; conv }
      | Value default -> category_of_value ~loc default conv
    and info_expr =
      let names_expr =
        let default_names_expr =
          let default_name_expr =
            Ast_builder.Default.estring ~loc:name.loc name.txt
          in
          [%expr [ [%e default_name_expr] ]]
        in
        term_attr.names
        |> Option.map Utils.expression_of_structure
        |> Option.value ~default:default_names_expr
      in
      Info.expr_of_term_attr ~loc names_expr term_attr
    in
    { category; info_expr }

  let to_expr ~loc ({ category; info_expr } : t) =
    let term_expr =
      match category with
      | Flag -> [%expr Cmdliner.Arg.value (Cmdliner.Arg.flag info)]
      | Flag_all -> [%expr Cmdliner.Arg.value (Cmdliner.Arg.flag_all info)]
      | Opt { default_expr; conv } ->
          let conv_expr = Conv.to_expr conv in
          [%expr
            Cmdliner.Arg.value
              (Cmdliner.Arg.opt [%e conv_expr] [%e default_expr] info)]
      | Opt_all { default_expr; conv } ->
          let conv_expr = Conv.to_expr conv in
          [%expr
            Cmdliner.Arg.value
              (Cmdliner.Arg.opt_all [%e conv_expr] [%e default_expr] info)]
      | Required conv ->
          let conv_expr = Conv.to_expr conv in
          [%expr
            Cmdliner.Arg.required
              (Cmdliner.Arg.opt (Cmdliner.Arg.some [%e conv_expr]) None info)]
      | Non_empty_opt conv ->
          let conv_expr = Conv.to_expr conv in
          [%expr Cmdliner.Arg.non_empty (Cmdliner.Arg.opt [%e conv_expr] [])]
      | Non_empty_opt_all conv ->
          let conv_expr = Conv.to_expr conv in
          [%expr
            Cmdliner.Arg.non_empty (Cmdliner.Arg.opt_all [%e conv_expr] [])]
      | Non_empty_flag_all ->
          [%expr Cmdliner.Arg.non_empty (Cmdliner.Arg.flag_all info)]
      | Last { default_expr; conv } ->
          let conv_expr = Conv.to_expr conv in
          [%expr
            Cmdliner.Arg.last
              (Cmdliner.Arg.opt_all [%e conv_expr] [%e default_expr])]
    in

    [%expr
      let info : Cmdliner.Arg.info = [%e info_expr] in
      [%e term_expr]]
end

module Positional = struct
  type category = Pos of int | Pos_all | Pos_left of int | Pos_right of int
  type t = { category : category; info_expr : expression }

  let of_term_attr ~loc:_ _name _ct (_term_attr : term_attr) : t = failwith ""
  let to_expr ~loc:_ (_ : t) = failwith ""
end

module T = struct
  let expr_of_term_attr ~loc name ct (term_attr : term_attr) =
    let pos_count =
      let count opt = if Option.is_some opt then 1 else 0 in
      count term_attr.pos
      + count term_attr.pos_all
      + count term_attr.pos_left
      + count term_attr.pos_right
    in
    match pos_count with
    (* named *)
    | 0 -> Named.of_term_attr ~loc name ct term_attr |> Named.to_expr ~loc
    (* positional *)
    | 1 ->
        Positional.of_term_attr ~loc name ct term_attr
        |> Positional.to_expr ~loc
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
          |> T.expr_of_term_attr ~loc ld.pld_name ld.pld_type
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
