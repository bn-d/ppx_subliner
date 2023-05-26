type foo = { my_arg : string } [@@deriving subliner]

type params = Foo of foo | Bar | Foobar of { my_arg : string }
[@@deriving subliner]

let handle = function
  | Foo { my_arg } -> print_endline ("Foo " ^ my_arg)
  | Bar -> print_endline "Bar"
  | Foobar { my_arg } -> print_endline ("Foobar" ^ my_arg)

(* {eval function}.{type name} <- {function expression> *)
[%%subliner.cmds
eval.params <- handle]
[@@name "foobar"]
[@@version "3.14"]
[@@default Cmdliner.Term.(ret (const (`Error (false, "foobar2000"))))]
(** Some docs *)
