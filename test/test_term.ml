open Ppxlib
module Ap = Ppx_subliner.Attribute_parser.Term

let loc = Location.none

let cmd term =
  let info = Cmdliner.Cmd.info "cmd" in
  Cmdliner.Cmd.v info Cmdliner.Term.(const Fun.id $ term ())

let test_ok prefix term = Utils.test_cmd_ok prefix (cmd term)
let test_error prefix term = Utils.test_cmd_error prefix (cmd term)

let test_raise =
  Utils.test_raises (fun (ct, attrs) ->
      let name = { txt = "field"; loc } in
      Ppx_subliner.Term.T.expr_of_attrs ~loc name ct attrs)

module Named = struct
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

  let test_set =
    let test_simple = test_ok "simple" simple_cmdliner_term
    and test_simple_error = test_error "simple" simple_cmdliner_term
    and test_opt_all = test_ok "opt_all" opt_all_cmdliner_term
    and test_opt_all_error = test_error "opt_all" opt_all_cmdliner_term in
    [
      test_simple "simple"
        {
          flag = true;
          default = 1;
          bool_default = false;
          option = Some [| 3.14; 3.15 |];
          required = Int32.of_int 21;
          non_empty = [ Int64.of_int 22; Int64.of_int 23 ];
          last = 'c';
          last_default = Nativeint.of_int 3;
        }
        [|
          "cmd";
          "--flag";
          "--default=1";
          "--bool-default=false";
          "--option=3.14,3.15";
          "--required=21";
          "--non-empty=22,23";
          "--last=a,b,c";
          "--last-default=1,2,3";
        |];
      test_simple "default"
        {
          flag = false;
          default = 42;
          bool_default = true;
          option = None;
          required = Int32.of_int 21;
          non_empty = [ Int64.of_int 22 ];
          last = 'a';
          last_default = Nativeint.of_int 42;
        }
        [| "cmd"; "--required=21"; "--non-empty=22"; "--last=a" |];
      test_simple_error "required" `Parse
        [| "cmd"; "--non-empty=22"; "--last=a,b,c" |];
      test_simple_error "non-empty" `Parse
        [| "cmd"; "--required=21"; "--last=a,b,c" |];
      test_simple_error "last" `Parse
        [| "cmd"; "--required=21"; "--non-empty=22" |];
      test_opt_all "simple"
        {
          required = [ true; false ];
          default = [ 1; 2 ];
          non_empty = [ 3; 4 ];
          last = 6;
          last_default = 8;
        }
        [|
          "cmd";
          "--required=true";
          "--required=false";
          "--default=1";
          "--default=2";
          "--non-empty=3";
          "--non-empty=4";
          "--last=5";
          "--last=6";
          "--last-default=7";
          "--last-default=8";
        |];
      test_opt_all "default"
        {
          required = [];
          default = [ 1; 2 ];
          non_empty = [ 1 ];
          last = 1;
          last_default = 4;
        }
        [| "cmd"; "--non-empty=1"; "--last=1" |];
      test_opt_all_error "non-empty" `Parse [| "cmd"; "--last=1" |];
      test_opt_all_error "last" `Parse [| "cmd"; "--non-empty=1" |];
    ]
end

module Positional = struct
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

  let test_set =
    let test_simple = test_ok "simple" simple_cmdliner_term
    and test_simple_error = test_error "simple" simple_cmdliner_term
    and test_left = test_ok "list_pos" left_cmdliner_term
    and test_right = test_ok "list_pos" right_cmdliner_term
    and test_right_error = test_error "list_pos" right_cmdliner_term
    and test_all = test_ok "list_pos" all_cmdliner_term in
    [
      test_simple "simple"
        {
          pos = 'a';
          last = "c";
          non_empty = [ 1; 2; 3 ];
          default = 1;
          option = Some 1.2;
          last_default = false;
        }
        [| "cmd"; "a"; "a,b,c"; "1,2,3"; "1"; "1.2"; "true,false" |];
      test_simple "default"
        {
          pos = 'b';
          last = "dd";
          non_empty = [ 4 ];
          default = 42;
          option = None;
          last_default = true;
        }
        [| "cmd"; "b"; "dd"; "4" |];
      test_simple_error "required" `Term [| "cmd" |];
      test_simple_error "non_empty" `Parse [| "cmd"; "a"; "a"; "" |];
      test_simple_error "too_many" `Parse
        [| "cmd"; "a"; "a"; "1"; "1.2"; "true,false"; "too_many" |];
      test_left "left_1" { last = 2 } [| "cmd"; "1"; "2" |];
      test_left "left_2" { last = 1 } [| "cmd"; "1" |];
      test_right "right"
        { non_empty = [ 2; 3; 4 ] }
        [| "cmd"; "1"; "2"; "3"; "4" |];
      test_right_error "non_empty" `Term [| "cmd" |];
      test_all "nested"
        { nested = [ [ 1 ]; [ 2 ]; [ 3 ] ] }
        [| "cmd"; "1"; "2"; "3" |];
      test_all "empty" { nested = [] } [| "cmd" |];
      test_ok "pos_list" rev_cmdliner_term "rev" { rev = [ 3 ] }
        [| "cmd"; "1"; "2"; "3" |];
    ]
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

let custom_conv = Cmdliner.Arg.(list ~sep:'@' int)

type conv = { conv : (int list[@conv custom_conv]) } [@@deriving subliner]

let test_set =
  let f = (loc, [%str]) and e = (loc, [%str expr]) in
  let test_names = test_ok "names" names_cmdliner_term in
  [
    test_names "long" { names = 1 } [| "cmd"; "--new_name"; "1" |];
    test_names "short" { names = 1 } [| "cmd"; "-n"; "1" |];
    test_ok "sep" sep_cmdliner_term "simple"
      {
        sep = [ 1; 2 ];
        list = [ 3; 4 ];
        array = [| 5; 6 |];
        tuple = (7, 8);
        nested = [ (0, 0, 0); (255, 255, 255) ];
        last_sep = 10;
      }
      [|
        "cmd";
        "--sep=1@2";
        "--list=3@4";
        "--array=5@6";
        "--tuple=7@8";
        "--nested=0#0#0;255#255#255";
        "--last-sep=9@10";
      |];
    test_ok "term" term_cmdliner_term "simple"
      { term = { names = 1 } }
      [| "cmd"; "--new_name"; "1" |];
    test_ok "conv" conv_cmdliner_term "simple"
      { conv = [ 1; 2; 3 ] }
      [| "cmd"; "--conv"; "1@2@3" |];
    test_raise "multi_pos"
      ~exn:
        "only one of `pos`, `pos_all`, `pos_left` and `pos_right` can be \
         specified at the same time"
      ([%type: int], Ap.make_t ~pos:e ~pos_all:f ());
    test_raise "pos_names_conflict"
      ~exn:"`names` cannot be used with positional argument"
      ([%type: int], Ap.make_t ~pos:f ~names:e ());
    test_raise "pos_opt_all_conflict"
      ~exn:"`opt_all` cannot be used with positional argument"
      ([%type: int], Ap.make_t ~pos:e ~opt_all:f ());
    test_raise "pos_all_rev_conflict" ~exn:"`rev` cannot be used with `pos_all`"
      ([%type: int], Ap.make_t ~pos_all:f ~rev:f ());
    test_raise "pos_sep_conflict"
      ~exn:"`sep` cannot be used with `pos_left`, `pos_right` and `pos_all`"
      ([%type: (int list[@sep ','])], Ap.make_t ~pos_all:f ());
    test_raise "pos.invalid"
      ~exn:"`pos_left`, `pos_right` and `pos_all` must be used with list type"
      ([%type: int], Ap.make_t ~pos_all:f ());
    test_raise "opt_all_sep_conflict"
      ~exn:"`opt_all` and `sep` cannot be used on the same list"
      ([%type: (int list[@sep ','])], Ap.make_t ~opt_all:f ());
    test_raise "opt_all.invalid" ~exn:"`opt_all` must be used with list type"
      ([%type: int], Ap.make_t ~opt_all:f ());
    test_raise "non_empty.invalid"
      ~exn:"`non_empty` must be used with list type"
      ([%type: int], Ap.make_t ~non_empty:f ());
  ]
