module P = Ppxlib
module Ast_helper = Ppxlib.Ast_helper

(* Misc. Utils *)

let unsupported_error ~loc str P.{ txt; loc = _ } =
  P.Location.raise_errorf ~loc "%s %s cannot be derived" str txt

let check_params_empty P.{ txt; loc } params =
  if List.length params == 0 then
    ()
  else
    P.Location.raise_errorf ~loc "type %s cannot have params" txt

let make_type_decl_generator f =
  P.Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
      let loc = P.Expansion_context.Deriver.derived_item_loc ctxt in
      tds |> List.map (f ~loc rec_flag) |> List.concat)

let longident_loc_of_name P.{ txt; loc } = P.{ txt = P.Lident txt; loc }
