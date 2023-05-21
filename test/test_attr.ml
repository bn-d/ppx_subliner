open Ppxlib
module Ap = Ppx_subliner.Attribute_parser

let loc = Location.none

module Term = struct
  module M = Ap.Term

  let test =
    Utils.test_equal Utils.pp (fun e ->
        e.pexp_attributes |> M.parse |> M.map (fun _ -> ()))

  let test_raises = Utils.test_raises (fun e -> M.parse e.pexp_attributes)

  let test_set =
    [
      test "empty" (M.make_t ()) [%expr t [@irrelevant]];
      test "deprecated" (M.make_t ~deprecated:() ()) [%expr t [@deprecated]];
      test "deprecated_" (M.make_t ~deprecated:() ()) [%expr t [@deprecated_]];
      test "absent" (M.make_t ~absent:() ()) [%expr t [@absent]];
      test "docs" (M.make_t ~docs:() ()) [%expr t [@docs]];
      test "docv" (M.make_t ~docv:() ()) [%expr t [@docv]];
      test "doc" (M.make_t ~doc:() ()) [%expr t [@doc]];
      test "env" (M.make_t ~env:() ()) [%expr t [@env]];
      test "env.deprecated"
        (M.make_t ~env_deprecated:() ())
        [%expr t [@env.deprecated]];
      test "env.docs" (M.make_t ~env_docs:() ()) [%expr t [@env.docs]];
      test "env.doc" (M.make_t ~env_doc:() ()) [%expr t [@env.doc]];
      test "names" (M.make_t ~names:() ()) [%expr t [@names]];
      test "opt_all" (M.make_t ~opt_all:() ()) [%expr t [@opt_all]];
      test "pos" (M.make_t ~pos:() ()) [%expr t [@pos]];
      test "pos_all" (M.make_t ~pos_all:() ()) [%expr t [@pos_all]];
      test "pos_left" (M.make_t ~pos_left:() ()) [%expr t [@pos_left]];
      test "pos_right" (M.make_t ~pos_right:() ()) [%expr t [@pos_right]];
      test "rev" (M.make_t ~rev:() ()) [%expr t [@rev]];
      test "non_empty" (M.make_t ~non_empty:() ()) [%expr t [@non_empty]];
      test "last" (M.make_t ~last:() ()) [%expr t [@last]];
      test "last.sep" (M.make_t ~last_sep:() ()) [%expr t [@last.sep]];
      test "default" (M.make_t ~default:() ()) [%expr t [@default]];
    ]
end

module Common = struct
  module M = Term.M

  let test = Term.test
  let test_raises = Term.test_raises
  let testf = Utils.testf (fun e -> e.pexp_attributes |> M.parse |> M.map snd)

  let test_set =
    [
      test "derived" (M.make_t ~doc:() ()) [%expr t [@ocaml.doc]];
      test "prefixed" (M.make_t ~doc:() ()) [%expr t [@subliner.doc]];
      (* level priority *)
      testf "priority_0"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@ocaml.doc] [@doc ""]];
      testf "priority_1"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@doc] [@subliner.doc ""]];
      testf "priority_2"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@ocaml.doc] [@subliner.doc ""]];
      testf "priority_3"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@doc] [@doc ""]];
      testf "priority_4"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@subliner.doc ""] [@doc]];
      (* expected failure *)
      test_raises "invalid_payload"
        ~exn:"this attribute payload must be an expression"
        [%expr t [@doc: int]];
      test_raises "invalid_attr" ~exn:"unexpected attribute name: irrelevant"
        [%expr t [@subliner.irrelevant]];
    ]
end

module String_conv = struct
  module M = Ap.String_conv

  let test =
    Utils.test_equal Utils.pp (fun e ->
        e.pexp_attributes |> M.parse |> M.map (fun _ -> ()))

  let test_set =
    [
      test "file" (M.make_t ~file:() ()) [%expr t [@file]];
      test "dir" (M.make_t ~dir:() ()) [%expr t [@dir]];
      test "non_dir_file"
        (M.make_t ~non_dir_file:() ())
        [%expr t [@non_dir_file]];
    ]
end

module Cmd_info = struct
  module M = Ap.Cmd_info

  let test =
    Utils.test_equal Utils.pp (fun e ->
        e.pexp_attributes |> M.parse |> M.map (fun _ -> ()))

  let test_set =
    [
      test "empty" (M.make_t ()) [%expr t [@irrelevant]];
      test "deprecated" (M.make_t ~deprecated:() ()) [%expr t [@deprecated]];
      test "deprecated_" (M.make_t ~deprecated:() ()) [%expr t [@deprecated_]];
      test "man_xrefs" (M.make_t ~man_xrefs:() ()) [%expr t [@man_xrefs]];
      test "man" (M.make_t ~man:() ()) [%expr t [@man]];
      test "envs" (M.make_t ~envs:() ()) [%expr t [@envs]];
      test "exits" (M.make_t ~exits:() ()) [%expr t [@exits]];
      test "sdocs" (M.make_t ~sdocs:() ()) [%expr t [@sdocs]];
      test "docs" (M.make_t ~docs:() ()) [%expr t [@docs]];
      test "doc" (M.make_t ~doc:() ()) [%expr t [@doc]];
      test "version" (M.make_t ~version:() ()) [%expr t [@version]];
      test "name" (M.make_t ~name:() ()) [%expr t [@name]];
    ]
end

module Single = struct
  let test f name =
    Utils.test_equal Utils.pp
      (fun e -> e.pexp_attributes |> f |> Option.map (fun _ -> ()))
      name

  let test_set =
    [
      test Ap.Sep_conv.parse "sep.empty" None [%expr t [@ocaml.doc]];
      test Ap.Sep_conv.parse "sep" (Some ()) [%expr t [@sep]];
      test Ap.Default_term.parse "default.empty" None [%expr t [@ocaml.doc]];
      test Ap.Default_term.parse "default" (Some ()) [%expr t [@default]];
    ]
end
