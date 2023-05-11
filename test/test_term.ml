open Ppxlib

let loc = Location.none

module Conv = struct
  open Ppx_subliner.Term.Conv

  let t = Alcotest.testable (fun _ _ -> ()) ( = )

  let test name (ct : core_type) (expected : complex) =
    let f () = of_core_type ct |> snd |> Alcotest.check t name expected in
    Alcotest.test_case name `Quick f

  let test_raises name (ct : core_type) expected =
    Utils.test_raises name expected (fun () -> of_core_type ct)

  let parse_test_set =
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
    ]

  let test_gen name (t : complex) (func : expression -> bool) =
    let f () = assert (to_expr (loc, t) |> func) in
    Alcotest.test_case name `Quick f

  let gen_test_set =
    [
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

let test_set = []
