open Ppxlib

let diff_msg = "actual is different from expected"
let pp _ _ = ()

let testf f name check input =
  let test () =
    let res = check @@ f input in
    if res then () else Alcotest.fail "check failed"
  in

  Alcotest.test_case name `Quick test

let test_equal pp f name expected input =
  let t = Alcotest.of_pp pp in
  let test () =
    let actual = f input in
    Alcotest.(check t) diff_msg expected actual
  in
  Alcotest.test_case name `Quick test

let test_raises f name ~exn input =
  let test () =
    try
      let () = f input |> ignore in
      Alcotest.fail "test expected to fail with exception"
    with Location.Error error ->
      let actual = Location.Error.message error in
      Alcotest.(check string) name exn actual
  in
  Alcotest.test_case name `Quick test

let eval_error_to_string : Cmdliner.Cmd.eval_error -> string = function
  | `Parse -> "A parse error occurred"
  | `Term -> "A term evaluation error occurred"
  | `Exn -> "An uncaught exception occurred"

let eval_error =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt @@ eval_error_to_string e)
    ( = )
