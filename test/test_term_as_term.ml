open Ppxlib
open Ppx_subliner.Term.As_term
module Attr = Ppx_subliner.Attribute_parser.Term

let loc = Location.none
let t = Alcotest.testable (fun _ _ -> ()) ( = )

let test name attrs expected =
  let f () = of_attrs ~loc attrs |> Alcotest.check t Utils.diff_msg expected in
  Alcotest.test_case name `Quick f

let test_raises name attrs expected =
  Utils.test_raises name expected (fun () -> of_attrs ~loc attrs)

let test_set =
  let unit_expr = [%expr ()] in
  let s = (loc, [%str]) and u = (loc, [%str [%e unit_expr]]) in
  [
    test "empty" Attr.empty (`value None);
    test "default" (Attr.make_t ~default:u ()) (`value (Some unit_expr));
    test_raises "default.invalid"
      (Attr.make_t ~default:s ())
      "unsupported payload for attribute";
    test "non_empty" (Attr.make_t ~non_empty:s ()) `non_empty;
    test_raises "non_empty.invalid"
      (Attr.make_t ~non_empty:u ())
      "flag cannot have any payload";
    test "last" (Attr.make_t ~last:s ()) (`last None);
    test "last.default"
      (Attr.make_t ~last:s ~default:u ())
      (`last (Some unit_expr));
    test_raises "last.invalid" (Attr.make_t ~last:u ())
      "flag cannot have any payload";
    test_raises "non_empty_last_conflict"
      (Attr.make_t ~non_empty:s ~last:s ())
      "`non_empty` and `last` cannot be used at the same time";
    test_raises "non_empty_default_conflict"
      (Attr.make_t ~non_empty:s ~default:u ())
      "`non_empty` and `default` cannot be used at the same time";
  ]
