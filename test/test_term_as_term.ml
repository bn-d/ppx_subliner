open Ppxlib
open Ppx_subliner.Term.As_term
module Attr = Ppx_subliner.Attribute_parser.Term

let loc = Location.none
let t = Alcotest.of_pp (fun _ _ -> ())
let test = Utils.test_equal t (of_attrs ~loc)
let test_raises = Utils.test_raises (of_attrs ~loc)

let test_set =
  let unit_expr = [%expr ()] in
  let s = (loc, [%str]) and u = (loc, [%str [%e unit_expr]]) in
  [
    test "empty" (`value None) Attr.empty;
    test "default" (`value (Some unit_expr)) (Attr.make_t ~default:u ());
    test_raises "default.invalid" ~exn:"unsupported payload for attribute"
      (Attr.make_t ~default:s ());
    test "non_empty" `non_empty (Attr.make_t ~non_empty:s ());
    test_raises "non_empty.invalid" ~exn:"flag cannot have any payload"
      (Attr.make_t ~non_empty:u ());
    test "last" (`last None) (Attr.make_t ~last:s ());
    test "last.default" (`last (Some unit_expr))
      (Attr.make_t ~last:s ~default:u ());
    test_raises "last.invalid" ~exn:"flag cannot have any payload"
      (Attr.make_t ~last:u ());
    test_raises "non_empty_last_conflict"
      ~exn:"`non_empty` and `last` cannot be used at the same time"
      (Attr.make_t ~non_empty:s ~last:s ());
    test_raises "non_empty_default_conflict"
      ~exn:"`non_empty` and `default` cannot be used at the same time"
      (Attr.make_t ~non_empty:s ~default:u ());
  ]
