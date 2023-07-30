open Ppxlib

let f = Location.raise_errorf

let unsupported_type ~loc type_ { txt = name; loc = _ } =
  f ~loc "%s %s cannot be derived" type_ name

let field_type ~loc = f ~loc "unsupported field type"
let attribute_name ~loc = f ~loc "unexpected attribute name: %s"
let attribute_payload ~loc = f ~loc "payload of `%s` must be an expression"
let attribute_flag ~loc = f ~loc "flag cannot have any payload"
let attr_list_type ~loc = f ~loc "`%s` must be used with list type"
let enum_payload ~loc = f ~loc "enum variant cannot have any payload"

let unexpected ~loc =
  f ~loc "congratulation for triggering this `impossible` error"
