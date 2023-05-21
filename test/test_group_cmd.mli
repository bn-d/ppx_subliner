type cmdliner_a = { a : string [@pos 0] } [@@deriving cmdliner]
type t = { ts : string } [@@deriving cmdliner]

module M : sig
  type m = { m : int } [@@deriving cmdliner]
end

type simple =
  | Simple_a of cmdliner_a
  | Simple_t of t  (** cmdliner has special naming rule for type t *)
  | Simple_m of M.m
  | Simple_name_attr of cmdliner_a [@name "override-name"]
  | Simple_no_arg
[@@deriving subliner]

val test_set : unit Alcotest.test_case list
