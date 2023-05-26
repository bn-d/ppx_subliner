type t = { t : string } [@@deriving subliner]

module M : sig
  type m = { m : int } [@@deriving subliner]
end

type cmdliner = { cmdliner : string [@pos 0] } [@@deriving cmdliner]

type simple =
  | Simple_t of t
  | Simple_m of M.m
  | Simple_name_attr of t [@name "override-name"]
  | Simple_no_arg
  | Simple_inline of { i : int }
  | Simple_cmdliner of cmdliner
[@@deriving subliner]

val test_set : unit Alcotest.test_case list
