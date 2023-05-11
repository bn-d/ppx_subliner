open Ppxlib

let unsupported_type ~loc type_ { txt = name; loc = _ } =
  Location.raise_errorf ~loc "%s %s cannot be derived" type_ name

let field_type ~loc = Location.raise_errorf ~loc "unsupported field type"

let attribute_name ~loc name =
  Location.raise_errorf ~loc "unexpected attribute name: %s" name

let attribute_payload ~loc =
  Location.raise_errorf ~loc "unsupported payload for attribute"

let attribute_flag ~loc =
  Location.raise_errorf ~loc "flag cannot have any payload"
