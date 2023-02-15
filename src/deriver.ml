open Ppxlib

(* generate structure for type declaration *)
let structure_of_type_decl ~loc:_ (_rec_flag : rec_flag) (td : type_declaration)
    : structure =
  let name = td.ptype_name
  and loc = td.ptype_loc in
  let () = Utils.check_params_empty name td.ptype_params in
  match td with
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      Group_cmd.structure_of_variant_choices ~loc name cds
  | { ptype_kind = Ptype_record _lds; _ } ->
      (* type t = {l: T; ...} *)
      Utils.unsupported_error ~loc "type declaration of record" name
  | _ -> Utils.unsupported_error ~loc "type declaration" name

(* generate signature for type declaration *)
let signature_of_type_decl ~loc:_ (_rec_flag : rec_flag) (td : type_declaration)
    : signature =
  let name = td.ptype_name and loc = td.ptype_loc in
  let () = Utils.check_params_empty name td.ptype_params in
  match td with
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      Group_cmd.signature_of_variant_choices ~loc name cds
  | { ptype_kind = Ptype_record _lds; _ } ->
      (* type t = {l: T; ...} *)
      Utils.unsupported_error ~loc "type declaration of record" name
  | _ -> Utils.unsupported_error ~loc "type declaration" name

let str_type_decl = Utils.make_type_decl_generator structure_of_type_decl
let sig_type_decl = Utils.make_type_decl_generator signature_of_type_decl
let deriver = Deriving.add "subliner" ~str_type_decl ~sig_type_decl
