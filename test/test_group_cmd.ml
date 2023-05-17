type cmdliner_a = { a : string [@pos 0] } [@@deriving cmdliner]
type t = { ts : string } [@@deriving cmdliner]

module M = struct
  type m = { m : int } [@@deriving cmdliner]
end

type simple =
  | Simple_a of cmdliner_a
  | Simple_t of t  (** cmdliner has special naming rule for type t *)
  | Simple_m of M.m
  | Simple_name_attr of cmdliner_a [@name "override-name"]
  | Simple_no_arg
[@@deriving subliner]

let t = Alcotest.testable (fun _ _ -> ()) ( = )

let test name expected argv =
  let f () =
    let msg = "actual result is unexpected"
    and cmd =
      let info = Cmdliner.Cmd.info "cmd" in
      Cmdliner.Cmd.group info (simple_cmdliner_group_cmds Fun.id)
    in
    match Cmdliner.Cmd.eval_value ~argv cmd with
    | Ok (`Ok actual) -> Alcotest.(check t) msg expected actual
    | Ok _ -> Alcotest.fail "unexpected eva_ok result"
    | Error _error -> Alcotest.fail "unexpected eval result"
  in
  Alcotest.test_case name `Quick f

let test_set =
  [
    test "simple_a"
      (Simple_a { a = "test-str-a" })
      [| "cmd"; "simple_a"; "test-str-a" |];
    test "simple_t"
      (Simple_t { ts = "test-str-t" })
      [| "cmd"; "simple_t"; "--ts"; "test-str-t" |];
    test "simple_m" (Simple_m { m = 42 }) [| "cmd"; "simple_m"; "-m"; "42" |];
    test "simple_name_attr"
      (Simple_name_attr { a = "test-str-c" })
      [| "cmd"; "override-name"; "test-str-c" |];
    test "simple_no_arg" Simple_no_arg [| "cmd"; "simple_no_arg" |];
  ]
