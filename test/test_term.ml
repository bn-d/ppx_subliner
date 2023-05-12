open Ppxlib
module A = Ppx_subliner.Attribute_parser.Term

type attrs = Ppx_subliner.Term.attrs

let loc = Location.none
let msg = "actual is different from expected"
let unit_expr = [%expr ()]

module Conv = struct
  module M = Ppx_subliner.Term.Conv

  let t = Alcotest.testable (fun _ _ -> ()) ( = )

  let test name (ct : core_type) (expected : M.complex) =
    let f () = M.of_core_type ct |> snd |> Alcotest.check t msg expected in
    Alcotest.test_case name `Quick f

  let test_raises name (ct : core_type) expected =
    Utils.test_raises name expected (fun () -> M.of_core_type ct)

  let test_gen name (t : M.complex) (func : expression -> bool) =
    let f () = assert (M.to_expr (loc, t) |> func) in
    Alcotest.test_case ("gen." ^ name) `Quick f

  let test_set =
    let open M in
    [
      test "bool" [%type: bool] (Basic Bool);
      test "Bool.t" [%type: Bool.t] (Basic Bool);
      test "char" [%type: char] (Basic Char);
      test "Char.t`" [%type: Char.t] (Basic Char);
      test "int" [%type: int] (Basic Int);
      test "Int.t" [%type: Int.t] (Basic Int);
      test "nativeint" [%type: nativeint] (Basic Nativeint);
      test "Nativeint.t" [%type: Nativeint.t] (Basic Nativeint);
      test "int32" [%type: int32] (Basic Int32);
      test "Int32.t" [%type: Int32.t] (Basic Int32);
      test "int64" [%type: int64] (Basic Int64);
      test "Int64.t" [%type: Int64.t] (Basic Int64);
      test "float" [%type: float] (Basic Float);
      test "Float.t" [%type: Float.t] (Basic Float);
      test "string" [%type: string] (Basic String);
      test "String.t" [%type: String.t] (Basic String);
      test "option" [%type: int option] (Option Int);
      test "Option.t" [%type: int Option.t] (Option Int);
      test "list" [%type: int list] (List { sep_expr = None; basic = Int });
      test "List.t" [%type: int List.t] (List { sep_expr = None; basic = Int });
      test "array" [%type: int array] (Array { sep_expr = None; basic = Int });
      test "Array.t" [%type: int Array.t]
        (Array { sep_expr = None; basic = Int });
      test_raises "invalid_1" [%type: int seq] "unsupported field type";
      test_raises "invalid_2" [%type: unit] "unsupported field type";
      test_gen "basic" (Basic Bool) (function
        | [%expr Cmdliner.Arg.(bool)] -> true
        | _ -> false);
      test_gen "option" (Option Char) (function
        | [%expr Cmdliner.Arg.(some char)] -> true
        | _ -> false);
      test_gen "list"
        (List { sep_expr = None; basic = Int })
        (function
          | [%expr Cmdliner.Arg.(list ~sep:',' int)] -> true | _ -> false);
      test_gen "array"
        (Array { sep_expr = Some [%expr ';']; basic = Float })
        (function
          | [%expr Cmdliner.Arg.(array ~sep:';' float)] -> true | _ -> false);
    ]
end

module Info = struct
  module M = Ppx_subliner.Term.Info

  let test_gen name (attrs : attrs) (func : expression -> bool) =
    let f () = assert (M.expr_of_attrs ~loc [%expr [ "NAME" ]] attrs |> func) in
    Alcotest.test_case ("gen." ^ name) `Quick f

  let test_set =
    let u = (loc, [%str ()]) in
    [
      test_gen "empty" A.empty (function
        | [%expr Cmdliner.Arg.info [ "NAME" ]] -> true
        | _ -> false);
      test_gen "all"
        (A.make_t ~deprecated:u ~absent:u ~docs:u ~docv:u ~doc:u ~env:u ())
        (function
        | [%expr
            Cmdliner.Arg.info ~deprecated:() ~absent:() ~docs:() ~docv:()
              ~doc:() ~env:() [ "NAME" ]] ->
            true
        | _ -> false);
    ]
end

module As_term = struct
  module M = Ppx_subliner.Term.As_term

  let t = Alcotest.testable (fun _ _ -> ()) ( = )

  let test name (attrs : attrs) expected =
    let f () = M.of_attrs ~loc attrs |> Alcotest.check t msg expected in
    Alcotest.test_case name `Quick f

  let test_raises name (attrs : attrs) expected =
    Utils.test_raises name expected (fun () -> M.of_attrs ~loc attrs)

  let test_set =
    let open M in
    let s = (loc, [%str]) and u = (loc, [%str [%e unit_expr]]) in
    [
      test "empty" A.empty (Value None);
      test "default" (A.make_t ~default:u ()) (Value (Some unit_expr));
      test_raises "default.invalid" (A.make_t ~default:s ())
        "unsupported payload for attribute";
      test "non_empty" (A.make_t ~non_empty:s ()) Non_empty;
      test_raises "non_empty.invalid" (A.make_t ~non_empty:u ())
        "flag cannot have any payload";
      test "last" (A.make_t ~last:s ()) (Last None);
      test "last.default"
        (A.make_t ~last:s ~default:u ())
        (Last (Some unit_expr));
      test_raises "last.invalid" (A.make_t ~last:u ())
        "flag cannot have any payload";
      test_raises "non_empty_last_conflict"
        (A.make_t ~non_empty:s ~last:s ())
        "`non_empty` and `last` cannot be used at the same time";
      test_raises "non_empty_default_conflict"
        (A.make_t ~non_empty:s ~default:u ())
        "`non_empty` and `default` cannot be used at the same time";
    ]
end

let test_set = []
