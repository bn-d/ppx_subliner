open Ppxlib
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term
module Attr = Ppx_subliner.Attribute_parser.Term

type attrs = Ppx_subliner.Term.attrs

let loc = Location.none
let msg = "actual is different from expected"

module Info = struct
  module M = Ppx_subliner.Term.Info

  let test_gen name (attrs : attrs) (func : expression -> bool) =
    let f () = assert (M.expr_of_attrs ~loc [%expr [ "NAME" ]] attrs |> func) in
    Alcotest.test_case ("gen." ^ name) `Quick f

  let test_set =
    let u = (loc, [%str ()]) in
    [
      test_gen "empty" Attr.empty (function
        | [%expr Cmdliner.Arg.info [ "NAME" ]] -> true
        | _ -> false);
      test_gen "all"
        (Attr.make_t ~deprecated:u ~absent:u ~docs:u ~docv:u ~doc:u ~env:u ())
        (function
        | [%expr
            Cmdliner.Arg.info ~deprecated:() ~absent:() ~docs:() ~docv:()
              ~doc:() ~env:() [ "NAME" ]] ->
            true
        | _ -> false);
    ]
end

let test check term prefix name expected argv =
  let f () =
    let cmd =
      let info = Cmd.info "cmd" in
      Cmd.v info Term.(const Fun.id $ term ())
    in
    Cmd.eval_value ~argv cmd |> check expected
  in
  Alcotest.test_case (prefix ^ "." ^ name) `Quick f

let test_equal term t prefix name expected argv =
  test
    (fun expected result ->
      match result with
      | Ok (`Ok actual) -> Alcotest.check t msg expected actual
      | Ok _ -> Alcotest.fail "unexpected eva_ok result"
      | Error e ->
          Alcotest.failf "eval error: %s" @@ Utils.eval_error_to_string e)
    term prefix name expected argv

let test_error term prefix name expected argv =
  test
    (fun expected actual ->
      match actual with
      | Error actual -> Alcotest.check Utils.eval_error msg expected actual
      | Ok _ -> Alcotest.fail "test expected to fail")
    term prefix name expected argv

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

  let simple = Alcotest.testable (fun _ _ -> ()) ( = )

  type left = { last : int [@pos_left 2] [@last] } [@@deriving subliner]

  let left = Alcotest.testable (fun _ _ -> ()) ( = )

  type right = { non_empty : int list [@pos_right 0] [@non_empty] }
  [@@deriving subliner]

  let right = Alcotest.testable (fun _ _ -> ()) ( = )

  type all = { nested : int list list [@pos_all] } [@@deriving subliner]

  let all = Alcotest.testable (fun _ _ -> ()) ( = )

  let test_set =
    let test_simple = test_equal simple_cmdliner_term simple "simple"
    and test_simple_error = test_error simple_cmdliner_term "simple"
    and test_left = test_equal left_cmdliner_term left "list_pos"
    and test_right = test_equal right_cmdliner_term right "list_pos"
    and test_right_error = test_error right_cmdliner_term "list_pos"
    and test_all = test_equal all_cmdliner_term all "list_pos" in
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
