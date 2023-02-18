type cmdliner_a = { a : string [@pos 0] } [@@deriving cmdliner]
type cmdliner_b = { b : string } [@@deriving cmdliner]

type simple =
  | Simple_a of cmdliner_a
  | Simple_b of cmdliner_b
  | Simple_name_attr of cmdliner_a [@name "override-name"]
  | Simple_no_arg
[@@deriving subliner]

let any = Alcotest.testable (fun _ _ -> ()) ( = )

let test name expected argv =
  let f () =
    let msg = "actual result is unexpected"
    and cmd =
      let info = Cmdliner.Cmd.info "cmd" in
      Cmdliner.Cmd.group info (make_simple_cmdliner_group_cmds Fun.id)
    in
    match Cmdliner.Cmd.eval_value ~argv cmd with
    | Ok actual -> Alcotest.(check any) msg expected actual
    | Error _error -> Alcotest.fail "unexpected eval result"
  in
  Alcotest.test_case name `Quick f

let test_set =
  [
    test "simple_a"
      (`Ok (Simple_a { a = "test-str-a" }))
      [| "cmd"; "simple_a"; "test-str-a" |];
    test "simple_b"
      (`Ok (Simple_b { b = "test-str-b" }))
      [| "cmd"; "simple_b"; "-b"; "test-str-b" |];
    test "simple_name_attr"
      (`Ok (Simple_name_attr { a = "test-str-c" }))
      [| "cmd"; "override-name"; "test-str-c" |];
    test "simple_no_arg" (`Ok Simple_no_arg) [| "cmd"; "simple_no_arg" |];
  ]
