type t = Enum_1 | Enum_2 [@names [ "override-name"; "o" ]]
[@@deriving subliner_enum]

let cmdliner_term () =
  let open Cmdliner in
  Arg.required @@ Arg.pos 0 (Arg.some cmdliner_conv) None (Cmdliner.Arg.info [])

let test = Test_term.test_ok "simple" cmdliner_term
let test_error = Test_term.test_error "simple" cmdliner_term

let test_set =
  [
    test "simple" Enum_1 [| "cmd"; "enum-1" |];
    test "names_1" Enum_2 [| "cmd"; "override-name" |];
    test "names_2" Enum_2 [| "cmd"; "o" |];
    test_error "invalid" `Parse [| "cmd"; "enum-2" |];
  ]
