open Ppxlib
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term
module Attr = Ppx_subliner.Attribute_parser.Term

type attrs = Ppx_subliner.Term.attrs

let loc = Location.none

module Info = struct
  module M = Ppx_subliner.Term.Info

  let test_gen =
    Utils.test_equal Utils.pp (M.expr_of_attrs ~loc [%expr [ "NAME" ]])

  let test_set =
    let u = (loc, [%str ()]) in
    [
      test_gen "empty" [%expr Cmdliner.Arg.info [ "NAME" ]] Attr.empty;
      test_gen "all"
        (let env_expr =
           [%expr Cmdliner.Cmd.Env.info ~deprecated:() ~docs:() ~doc:() ()]
         in
         [%expr
           Cmdliner.Arg.info ~deprecated:() ~absent:() ~docs:() ~docv:() ~doc:()
             ~env:[%e env_expr] [ "NAME" ]])
        (Attr.make_t ~deprecated:u ~absent:u ~docs:u ~docv:u ~doc:u ~env:u
           ~env_deprecated:u ~env_docs:u ~env_doc:u ());
    ]
end

let test prefix check term name expected argv =
  let f () =
    let cmd =
      let info = Cmd.info "cmd" in
      Cmd.v info Term.(const Fun.id $ term ())
    in
    Cmd.eval_value ~argv cmd |> check expected
  in
  Alcotest.test_case (prefix ^ "." ^ name) `Quick f

let test_equal prefix term name expected argv =
  let t = Alcotest.of_pp Utils.pp in
  test prefix
    (fun expected result ->
      match result with
      | Ok (`Ok actual) -> Alcotest.check t Utils.diff_msg expected actual
      | Ok _ -> Alcotest.fail "unexpected eva_ok result"
      | Error e ->
          Alcotest.failf "eval error: %s" @@ Utils.eval_error_to_string e)
    term name expected argv

let test_error prefix term name expected argv =
  test prefix
    (fun expected actual ->
      match actual with
      | Error actual ->
          Alcotest.check Utils.eval_error Utils.diff_msg expected actual
      | Ok _ -> Alcotest.fail "test expected to fail")
    term name expected argv

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
    let test_simple = test_equal "simple" simple_cmdliner_term
    and test_simple_error = test_error "simple" simple_cmdliner_term
    and test_opt_all = test_equal "opt_all" opt_all_cmdliner_term
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

  let test_set =
    let test_simple = test_equal "simple" simple_cmdliner_term
    and test_simple_error = test_error "simple" simple_cmdliner_term
    and test_left = test_equal "list_pos" left_cmdliner_term
    and test_right = test_equal "list_pos" right_cmdliner_term
    and test_right_error = test_error "list_pos" right_cmdliner_term
    and test_all = test_equal "list_pos" all_cmdliner_term in
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
    ]
end

type names = { names : int [@names [ "new_name"; "n" ]] } [@@deriving subliner]

type sep = {
  sep : (int list[@sep '@']);
  list : (int list[@sep '@']);
  array : (int array[@sep '@']);
  tuple : (int * int[@sep '@']);
  nested : (((int * int * int)[@sep '#']) list[@sep ';']);
}
[@@deriving subliner]

let test_set =
  let test_names = test_equal "names" names_cmdliner_term
  and test_sep = test_equal "sep" sep_cmdliner_term in
  [
    test_names "long" { names = 1 } [| "cmd"; "--new_name"; "1" |];
    test_names "short" { names = 1 } [| "cmd"; "-n"; "1" |];
    test_sep "simple"
      {
        sep = [ 1; 2 ];
        list = [ 3; 4 ];
        array = [| 5; 6 |];
        tuple = (7, 8);
        nested = [ (0, 0, 0); (255, 255, 255) ];
      }
      [|
        "cmd";
        "--sep=1@2";
        "--list=3@4";
        "--array=5@6";
        "--tuple=7@8";
        "--nested=0#0#0;255#255#255";
      |];
  ]
