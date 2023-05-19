open Ppxlib
open Ppx_subliner.Term.Conv
module Attr = Ppx_subliner.Attribute_parser.Term

let loc = Location.none
let t = Alcotest.of_pp (fun _ _ -> ())
let test = Utils.test_equal t of_core_type
let test_raises = Utils.test_raises of_core_type
let test_gen name = Utils.testf (to_expr ~loc Attr.empty) ("gen." ^ name)

let test_set =
  [
    test "bool" Bool [%type: bool];
    test "Bool.t" Bool [%type: Bool.t];
    test "char" Char [%type: char];
    test "Char.t`" Char [%type: Char.t];
    test "int" Int [%type: int];
    test "Int.t" Int [%type: Int.t];
    test "nativeint" Nativeint [%type: nativeint];
    test "Nativeint.t" Nativeint [%type: Nativeint.t];
    test "int32" Int32 [%type: int32];
    test "Int32.t" Int32 [%type: Int32.t];
    test "int64" Int64 [%type: int64];
    test "Int64.t" Int64 [%type: Int64.t];
    test "float" Float [%type: float];
    test "Float.t" Float [%type: Float.t];
    test "string" String [%type: string];
    test "String.t" String [%type: String.t];
    test "option" (Option Int) [%type: int option];
    test "Option.t" (Option Int) [%type: int Option.t];
    test "list" (List Int) [%type: int list];
    test "List.t" (List Int) [%type: int List.t];
    test "array" (Array Int) [%type: int array];
    test "Array.t" (Array Int) [%type: int Array.t];
    test "nested" (List (List Int)) [%type: int list list];
    test_raises "invalid_1" ~exn:"unsupported field type" [%type: int seq];
    test_raises "invalid_2" ~exn:"unsupported field type" [%type: unit];
    test_gen "basic"
      (function [%expr Cmdliner.Arg.(bool)] -> true | _ -> false)
      Bool;
    test_gen "option"
      (function [%expr Cmdliner.Arg.(some char)] -> true | _ -> false)
      (Option Char);
    test_gen "list"
      (function
        | [%expr Cmdliner.Arg.(list ?sep:None int)] -> true | _ -> false)
      (List Int);
    test_gen "array"
      (function
        | [%expr Cmdliner.Arg.(array ?sep:None int)] -> true | _ -> false)
      (Array Int);
  ]
