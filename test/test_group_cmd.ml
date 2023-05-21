type t = { t : string } [@@deriving subliner]

module M = struct
  type m = { m : int } [@@deriving subliner]
end

type cmdliner = { cmdliner : string [@pos 0] } [@@deriving cmdliner]

type simple =
  | Simple_t of t  (** special naming rule for type t *)
  | Simple_m of M.m
  | Simple_name_attr of t [@name "override-name"]
  | Simple_no_arg
  | Simple_cmdliner of cmdliner
[@@deriving subliner]

let test =
  let cmd =
    let info = Cmdliner.Cmd.info "cmd" in
    Cmdliner.Cmd.group info (simple_cmdliner_group_cmds Fun.id)
  in
  Utils.test_cmd_ok "simple" cmd

let test_set =
  [
    test "simple_t"
      (Simple_t { t = "test-str-t" })
      [| "cmd"; "simple-t"; "-t"; "test-str-t" |];
    test "simple_m" (Simple_m { m = 42 }) [| "cmd"; "simple-m"; "-m"; "42" |];
    test "simple_name_attr"
      (Simple_name_attr { t = "test-str" })
      [| "cmd"; "override-name"; "-t"; "test-str" |];
    test "simple_no_arg" Simple_no_arg [| "cmd"; "simple-no-arg" |];
    test "simple_cmdliner"
      (Simple_cmdliner { cmdliner = "test-str-cmdliner" })
      [| "cmd"; "simple-cmdliner"; "test-str-cmdliner" |];
  ]
