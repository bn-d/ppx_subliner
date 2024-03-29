open Ppxlib
open Ppx_subliner.Term.As_term
module Ap = Ppx_subliner.Attribute_parser.Term

let loc = Location.none
let test = Utils.test_equal Utils.pp (of_attrs ~loc)
let test_raises = Utils.test_raises (of_attrs ~loc)

let test_set =
  let unit_expr = [%expr ()] in
  let s = (loc, [%str]) and u = (loc, [%str [%e unit_expr]]) in
  [
    test "empty" (`value None) Ap.empty;
    test "default" (`value (Some unit_expr)) (Ap.make_t ~default:u ());
    test_raises "default.invalid"
      ~exn:"payload of `default` must be an expression"
      (Ap.make_t ~default:s ());
    test "non_empty" `non_empty (Ap.make_t ~non_empty:s ());
    test_raises "non_empty.invalid" ~exn:"flag cannot have any payload"
      (Ap.make_t ~non_empty:u ());
    test "last" (`last (None, None)) (Ap.make_t ~last:s ());
    test "last.default"
      (`last (None, Some unit_expr))
      (Ap.make_t ~last:s ~default:u ());
    test "last.sep"
      (`last (Some unit_expr, None))
      (Ap.make_t ~last:s ~last_sep:u ());
    test_raises "last.invalid" ~exn:"flag cannot have any payload"
      (Ap.make_t ~last:u ());
    test_raises "non_empty_last_conflict"
      ~exn:"`non_empty` and `last` cannot be used at the same time"
      (Ap.make_t ~non_empty:s ~last:s ());
    test_raises "non_empty_default_conflict"
      ~exn:"`non_empty` and `default` cannot be used at the same time"
      (Ap.make_t ~non_empty:s ~default:u ());
  ]
