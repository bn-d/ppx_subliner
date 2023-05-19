open Ppxlib
open Ppx_subliner.Term.Conv
module Attr = Ppx_subliner.Attribute_parser.Term

let loc = Location.none
let t = Alcotest.of_pp (fun _ _ -> ())
let expr = Alcotest.of_pp (fun _ _ -> ())
let test = Utils.test_equal t of_core_type
let test_raises = Utils.test_raises of_core_type

let test_gen name =
  Utils.test_equal expr (to_expr ~loc Attr.empty) ("gen." ^ name)

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
    test "pair" (Pair (Int, Float)) [%type: int * float];
    test "t3" (T3 (Int, Float, Char)) [%type: int * float * char];
    test "t4" (T4 (Int, Float, Char, Bool)) [%type: int * float * char * bool];
    test "nested" (List (List Int)) [%type: int list list];
    test_raises "invalid_1" ~exn:"unsupported field type" [%type: int seq];
    test_raises "invalid_2" ~exn:"unsupported field type" [%type: unit];
    test_gen "basic" [%expr Cmdliner.Arg.(bool)] Bool;
    test_gen "option" [%expr Cmdliner.Arg.(some char)] (Option Char);
    test_gen "list" [%expr Cmdliner.Arg.(list ?sep:None int)] (List Int);
    test_gen "array" [%expr Cmdliner.Arg.(array ?sep:None int)] (Array Int);
    test_gen "pair"
      [%expr Cmdliner.Arg.(pair ?sep:None int float)]
      (Pair (Int, Float));
    test_gen "t3"
      [%expr Cmdliner.Arg.(t3 ?sep:None int float char)]
      (T3 (Int, Float, Char));
    test_gen "t4"
      [%expr Cmdliner.Arg.(t4 ?sep:None int float char bool)]
      (T4 (Int, Float, Char, Bool));
  ]
