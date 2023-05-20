open Ppxlib

let loc = Location.none

module Term = struct
  module M = Ppx_subliner.Attribute_parser.Term

  let test =
    Utils.test_equal Utils.pp (fun e ->
        e.pexp_attributes |> M.parse |> M.map (fun _ -> ()))

  let test_raises = Utils.test_raises (fun e -> M.parse e.pexp_attributes)

  let test_set =
    [
      test "empty" (M.make_t ()) [%expr t [@irrelevant]];
      test "deprecated" (M.make_t ~deprecated:() ()) [%expr t [@deprecated]];
      test "deprecated.s"
        (M.make_t ~deprecated:() ())
        [%expr t [@subliner.deprecated]];
      test "deprecated_" (M.make_t ~deprecated:() ()) [%expr t [@deprecated_]];
      test "deprecated_.s"
        (M.make_t ~deprecated:() ())
        [%expr t [@subliner.deprecated_]];
      test "absent" (M.make_t ~absent:() ()) [%expr t [@absent]];
      test "absent.s" (M.make_t ~absent:() ()) [%expr t [@subliner.absent]];
      test "doc" (M.make_t ~doc:() ()) [%expr t [@doc]];
      test "doc.s" (M.make_t ~doc:() ()) [%expr t [@subliner.doc]];
      test "ocaml.doc" (M.make_t ~doc:() ()) [%expr t [@ocaml.doc]];
      test "docs" (M.make_t ~docs:() ()) [%expr t [@docs]];
      test "docs.s" (M.make_t ~docs:() ()) [%expr t [@subliner.docs]];
      test "docv" (M.make_t ~docv:() ()) [%expr t [@docv]];
      test "docv.s" (M.make_t ~docv:() ()) [%expr t [@subliner.docv]];
      test "env" (M.make_t ~env:() ()) [%expr t [@env]];
      test "env.s" (M.make_t ~env:() ()) [%expr t [@subliner.env]];
      test "names" (M.make_t ~names:() ()) [%expr t [@names]];
      test "names.s" (M.make_t ~names:() ()) [%expr t [@subliner.names]];
      test "opt_all" (M.make_t ~opt_all:() ()) [%expr t [@opt_all]];
      test "opt_all.s" (M.make_t ~opt_all:() ()) [%expr t [@subliner.opt_all]];
      test "pos" (M.make_t ~pos:() ()) [%expr t [@pos]];
      test "pos.s" (M.make_t ~pos:() ()) [%expr t [@subliner.pos]];
      test "pos_all" (M.make_t ~pos_all:() ()) [%expr t [@pos_all]];
      test "pos_all.s" (M.make_t ~pos_all:() ()) [%expr t [@subliner.pos_all]];
      test "pos_left" (M.make_t ~pos_left:() ()) [%expr t [@pos_left]];
      test "pos_left.s" (M.make_t ~pos_left:() ())
        [%expr t [@subliner.pos_left]];
      test "pos_right" (M.make_t ~pos_right:() ()) [%expr t [@pos_right]];
      test "pos_right.s"
        (M.make_t ~pos_right:() ())
        [%expr t [@subliner.pos_right]];
      test "non_empty" (M.make_t ~non_empty:() ()) [%expr t [@non_empty]];
      test "non_empty.s"
        (M.make_t ~non_empty:() ())
        [%expr t [@subliner.non_empty]];
      test "last" (M.make_t ~last:() ()) [%expr t [@last]];
      test "last.s" (M.make_t ~last:() ()) [%expr t [@subliner.last]];
      test "default" (M.make_t ~default:() ()) [%expr t [@default]];
      test "default.s" (M.make_t ~default:() ()) [%expr t [@subliner.default]];
    ]
end

module Common = struct
  module M = Term.M

  let test_raises = Term.test_raises
  let test = Utils.testf (fun e -> e.pexp_attributes |> M.parse |> M.map snd)

  let test_set =
    [
      (* level priority *)
      test "priority_0"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@ocaml.doc] [@doc ""]];
      test "priority_1"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@doc] [@subliner.doc ""]];
      test "priority_2"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@ocaml.doc] [@subliner.doc ""]];
      test "priority_3"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@doc] [@doc ""]];
      test "priority_4"
        (fun { doc; _ } -> doc |> Option.get |> List.length |> ( = ) 1)
        [%expr t [@subliner.doc ""] [@doc]];
      (* expected failure *)
      test_raises "invalid_payload" ~exn:"unsupported payload for attribute"
        [%expr t [@doc: int]];
      test_raises "invalid_attr" ~exn:"unexpected attribute name: irrelevant"
        [%expr t [@subliner.irrelevant]];
    ]
end

module Cmd_info = struct
  module M = Ppx_subliner.Attribute_parser.Cmd_info

  let test =
    Utils.test_equal Utils.pp (fun e ->
        e.pexp_attributes |> M.parse |> M.map (fun _ -> ()))

  let test_set =
    [
      test "empty" (M.make_t ()) [%expr t [@irrelevant]];
      test "deprecated" (M.make_t ~deprecated:() ()) [%expr t [@deprecated]];
      test "deprecated.s"
        (M.make_t ~deprecated:() ())
        [%expr t [@subliner.deprecated]];
      test "deprecated_" (M.make_t ~deprecated:() ()) [%expr t [@deprecated_]];
      test "deprecated_.s"
        (M.make_t ~deprecated:() ())
        [%expr t [@subliner.deprecated_]];
      test "man_xrefs" (M.make_t ~man_xrefs:() ()) [%expr t [@man_xrefs]];
      test "man_xrefs.s"
        (M.make_t ~man_xrefs:() ())
        [%expr t [@subliner.man_xrefs]];
      test "man" (M.make_t ~man:() ()) [%expr t [@man]];
      test "man.s" (M.make_t ~man:() ()) [%expr t [@subliner.man]];
      test "envs" (M.make_t ~envs:() ()) [%expr t [@envs]];
      test "envs.s" (M.make_t ~envs:() ()) [%expr t [@subliner.envs]];
      test "exits" (M.make_t ~exits:() ()) [%expr t [@exits]];
      test "exits.s" (M.make_t ~exits:() ()) [%expr t [@subliner.exits]];
      test "sdocs" (M.make_t ~sdocs:() ()) [%expr t [@sdocs]];
      test "sdocs.s" (M.make_t ~sdocs:() ()) [%expr t [@subliner.sdocs]];
      test "docs" (M.make_t ~docs:() ()) [%expr t [@docs]];
      test "docs.s" (M.make_t ~docs:() ()) [%expr t [@subliner.docs]];
      test "doc" (M.make_t ~doc:() ()) [%expr t [@doc]];
      test "doc.s" (M.make_t ~doc:() ()) [%expr t [@subliner.doc]];
      test "ocaml.doc" (M.make_t ~doc:() ()) [%expr t [@ocaml.doc]];
      test "version" (M.make_t ~version:() ()) [%expr t [@version]];
      test "version.s" (M.make_t ~version:() ()) [%expr t [@subliner.version]];
      test "name" (M.make_t ~name:() ()) [%expr t [@name]];
      test "name.s" (M.make_t ~name:() ()) [%expr t [@subliner.name]];
    ]
end
