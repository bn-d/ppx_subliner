open Ppxlib

(* compatibility *)
let rec list_find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> list_find_map f l)

let string_starts_with ~prefix s =
  let len_s = String.length s and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then
      true
    else if String.unsafe_get s i <> String.unsafe_get prefix i then
      false
    else
      aux (i + 1)
  in
  len_s >= len_pre && aux 0

(* Misc. Utils *)

let unsupported_error ~loc str { txt; loc = _ } =
  Location.raise_errorf ~loc "%s %s cannot be derived" str txt

let check_params_empty { txt; loc } params =
  if List.length params == 0 then
    ()
  else
    Location.raise_errorf ~loc "type %s cannot have params" txt

let make_type_decl_generator f =
  Deriving.Generator.V2.make_noarg (fun ~ctxt (rec_flag, tds) ->
      let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      tds |> List.map (f ~loc rec_flag) |> List.concat)

let longident_loc_of_name { txt; loc } = { txt = Lident txt; loc }

let map_lid_name f { txt; loc } =
  let rec impl = function
    | Lident str -> Lident (f str)
    | Ldot (t, str) -> Ldot (impl t, str)
    | _ -> Location.raise_errorf ~loc "Lapply of Longident is not supported"
  in
  { txt = impl txt; loc }
