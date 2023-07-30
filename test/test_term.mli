val test_ok :
  string ->
  (unit -> 'a Cmdliner.Term.t) ->
  string ->
  'a ->
  string array ->
  unit Alcotest.test_case

val test_error :
  string ->
  (unit -> 'a Cmdliner.Term.t) ->
  string ->
  Cmdliner.Cmd.eval_error ->
  string array ->
  unit Alcotest.test_case

module Named : sig
  type simple = {
    flag : bool;
    default : int; [@default 42]
    bool_default : bool; [@default true]
    option : float array option;
    required : int32;
    non_empty : int64 list; [@non_empty]
    last : char; [@last]
    last_default : nativeint; [@last] [@default Nativeint.of_int 42]
  }
  [@@deriving subliner]

  type opt_all = {
    required : bool list; [@opt_all]
    default : int list; [@opt_all] [@default [ 1; 2 ]]
    non_empty : int list; [@opt_all] [@non_empty]
    last : int; [@opt_all] [@last]
    last_default : int; [@opt_all] [@last] [@default 4]
  }
  [@@deriving subliner]

  val test_set : unit Alcotest.test_case list
end

module Positional : sig
  type simple = {
    pos : char; [@pos 0]
    last : string; [@pos 1] [@last]
    non_empty : int list; [@pos 2] [@non_empty]
    default : int; [@pos 3] [@default 42]
    option : float option; [@pos 4]
    last_default : bool; [@pos 5] [@last] [@default true]
  }
  [@@deriving subliner]

  type left = { last : int [@pos_left 2] [@last] } [@@deriving subliner]

  type right = { non_empty : int list [@pos_right 0] [@non_empty] }
  [@@deriving subliner]

  type all = { nested : int list list [@pos_all] } [@@deriving subliner]
  type rev = { rev : int list [@pos 0] [@rev] } [@@deriving subliner]

  val test_set : unit Alcotest.test_case list
end

type names = { names : int [@names [ "new_name"; "n" ]] } [@@deriving subliner]

type sep = {
  sep : (int list[@sep '@']);
  list : (int list[@sep '@']);
  array : (int array[@sep '@']);
  tuple : (int * int[@sep '@']);
  nested : (((int * int * int)[@sep '#']) list[@sep ';']);
  last_sep : int; [@last] [@last.sep '@']
}
[@@deriving subliner]

type term = { term : names [@term names_cmdliner_term ()] }
[@@deriving subliner]

type conv = { conv : (int list[@conv custom_conv]) } [@@deriving subliner]

val test_set : unit Alcotest.test_case list
