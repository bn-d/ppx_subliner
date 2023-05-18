open Ppxlib

let f = Location.raise_errorf

let unsupported_type ~loc type_ { txt = name; loc = _ } =
  f ~loc "%s %s cannot be derived" type_ name

let field_type ~loc = f ~loc "unsupported field type"
let attribute_name ~loc name = f ~loc "unexpected attribute name: %s" name
let attribute_payload ~loc = f ~loc "unsupported payload for attribute"
let attribute_flag ~loc = f ~loc "flag cannot have any payload"
let non_empty_list ~loc = f ~loc "`non_empty` can only be used with `list`"

let unexpected ~loc =
  f ~loc "congratulation for triggering this `impossible` error"
