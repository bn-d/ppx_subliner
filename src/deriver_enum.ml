open Ppxlib

(* generate structure for type declaration *)
let structure_of_type_decl ~loc:_ (_ : rec_flag) (td : type_declaration) :
    structure =
  let name = td.ptype_name and loc = td.ptype_loc in
  let () = Utils.check_params_empty name td.ptype_params in
  match td with
  | { ptype_kind = Ptype_variant cds; _ } ->
      (* type t = C of T | ... *)
      Enum.structure_of_const_decls ~loc name cds
  | _ -> Error.unsupported_type ~loc "type declaration" name

(* generate signature for type declaration *)
let signature_of_type_decl ~loc:_ (_ : rec_flag) (td : type_declaration) :
    signature =
  let name = td.ptype_name and loc = td.ptype_loc in
  let () = Utils.check_params_empty name td.ptype_params in
  match td with
  | { ptype_kind = Ptype_variant _; _ } ->
      (* type t = C of T | ... *)
      Enum.signature_of_const_decls ~loc name
  | _ -> Error.unsupported_type ~loc "type declaration" name

let str_type_decl = Utils.make_type_decl_generator structure_of_type_decl
let sig_type_decl = Utils.make_type_decl_generator signature_of_type_decl
let deriver = Deriving.add "subliner_enum" ~str_type_decl ~sig_type_decl
