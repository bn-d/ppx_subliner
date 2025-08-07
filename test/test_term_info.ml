open Ppxlib
open Ppx_subliner.Term.Info
module Ap = Ppx_subliner.Attribute_parser.Term

let loc = Location.none

let test_gen =
  Utils.test_equal Ppxlib.Pprintast.expression
    (expr_of_attrs ~loc [%expr [ "NAME" ]])

let test_set =
  let u = (loc, [%str ()]) in
  [
    test_gen "empty" [%expr Cmdliner.Arg.info [ "NAME" ]] Ap.empty;
    test_gen "all"
      (let env_expr =
         [%expr Cmdliner.Cmd.Env.info ~deprecated:() ~docs:() ~doc:() ()]
       in
       [%expr
         Cmdliner.Arg.info ~deprecated:() ~absent:() ~docs:() ~docv:()
           ~doc:(Stdlib.String.trim ()) ~env:[%e env_expr] [ "NAME" ]])
      (Ap.make_t ~deprecated:u ~absent:u ~docs:u ~docv:u ~doc:u ~env:u
         ~env_deprecated:u ~env_docs:u ~env_doc:u ());
  ]
