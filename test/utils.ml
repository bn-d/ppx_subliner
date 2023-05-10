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
