open Ppxlib

let diff_msg = "actual is different from expected"
let pp _ _ = ()

let eval_error_to_string : Cmdliner.Cmd.eval_error -> string = function
  | `Parse -> "A parse error occurred"
  | `Term -> "A term evaluation error occurred"
  | `Exn -> "An uncaught exception occurred"

let eval_error =
  Alcotest.of_pp (fun fmt e ->
      Format.pp_print_string fmt @@ eval_error_to_string e)

let eval pp =
  Alcotest.of_pp (fun fmt o ->
      match o with
      | Ok (`Ok t) -> Format.fprintf fmt "`ok(%a)" pp t
      | Ok `Version -> Format.pp_print_string fmt "`version"
      | Ok `Help -> Format.pp_print_string fmt "`help"
      | Error e -> Format.fprintf fmt "`error(%s)" @@ eval_error_to_string e)

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

let test_cmd prefix cmd name expected argv =
  let f () =
    Cmdliner.Cmd.eval_value ~argv cmd
    |> Alcotest.check (eval pp) diff_msg expected
  in
  Alcotest.test_case (prefix ^ "." ^ name) `Quick f

let test_cmd_ok prefix cmd name expected argv =
  test_cmd prefix cmd name (Ok (`Ok expected)) argv

let test_cmd_error prefix cmd name expected argv =
  test_cmd prefix cmd name (Error expected) argv
