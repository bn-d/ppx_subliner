open Ppxlib

let test_raises name expected f =
  let impl () =
    try
      let () = f () |> ignore in
      Alcotest.fail "test expected to fail with exception"
    with Location.Error error ->
      let actual = Location.Error.message error in
      Alcotest.(check string) name expected actual
  in
  Alcotest.test_case name `Quick impl

let eval_error_to_string : Cmdliner.Cmd.eval_error -> string = function
  | `Parse -> "A parse error occurred"
  | `Term -> "A term evaluation error occurred"
  | `Exn -> "An uncaught exception occurred"

let eval_error =
  Alcotest.testable
    (fun fmt e -> Format.pp_print_string fmt @@ eval_error_to_string e)
    ( = )
