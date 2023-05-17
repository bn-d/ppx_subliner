open Ppxlib

let loc = Location.none

let test_impl
    (parser : attributes -> 'a)
    name
    (check : 'a -> unit)
    (expression : expression) =
  let f () = expression.pexp_attributes |> parser |> check in
  Alcotest.test_case name `Quick f

module Term = struct
  module M = Ppx_subliner.Attribute_parser.Term

  let test =
    test_impl (fun expr ->
        M.(parse expr |> map (fun (_loc, structure) -> structure)))

  let test_exist name func expr =
    test ("attr." ^ name) (fun t -> assert (t |> func |> Option.is_some)) expr

  let test_raises name ~exn (expression : expression) =
    Utils.test_raises name ~exn (fun () -> M.parse expression.pexp_attributes)

  let test_set =
    [
      test "empty"
        (fun t -> M.map (fun _ -> assert false) t |> ignore)
        [%expr t];
      test_exist "deprecated"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@deprecated]];
      test_exist "deprecated.s"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@subliner.deprecated]];
      test_exist "deprecated_"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@deprecated_]];
      test_exist "deprecated_.s"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@subliner.deprecated_]];
      test_exist "absent" (fun { absent = v; _ } -> v) [%expr t [@absent]];
      test_exist "absent.s"
        (fun { absent = v; _ } -> v)
        [%expr t [@subliner.absent]];
      test_exist "doc" (fun { doc = v; _ } -> v) [%expr t [@doc]];
      test_exist "doc.s" (fun { doc = v; _ } -> v) [%expr t [@subliner.doc]];
      test_exist "ocaml.doc" (fun { doc = v; _ } -> v) [%expr t [@ocaml.doc]];
      test_exist "docs" (fun { docs = v; _ } -> v) [%expr t [@docs]];
      test_exist "docs.s" (fun { docs = v; _ } -> v) [%expr t [@subliner.docs]];
      test_exist "docv" (fun { docv = v; _ } -> v) [%expr t [@docv]];
      test_exist "docv.s" (fun { docv = v; _ } -> v) [%expr t [@subliner.docv]];
      test_exist "env" (fun { env = v; _ } -> v) [%expr t [@env]];
      test_exist "env.s" (fun { env = v; _ } -> v) [%expr t [@subliner.env]];
      test_exist "names" (fun { names = v; _ } -> v) [%expr t [@names]];
      test_exist "names.s"
        (fun { names = v; _ } -> v)
        [%expr t [@subliner.names]];
      test_exist "pos" (fun { pos = v; _ } -> v) [%expr t [@pos]];
      test_exist "pos.s" (fun { pos = v; _ } -> v) [%expr t [@subliner.pos]];
      test_exist "pos_all" (fun { pos_all = v; _ } -> v) [%expr t [@pos_all]];
      test_exist "pos_all.s"
        (fun { pos_all = v; _ } -> v)
        [%expr t [@subliner.pos_all]];
      test_exist "pos_left" (fun { pos_left = v; _ } -> v) [%expr t [@pos_left]];
      test_exist "pos_left.s"
        (fun { pos_left = v; _ } -> v)
        [%expr t [@subliner.pos_left]];
      test_exist "pos_right"
        (fun { pos_right = v; _ } -> v)
        [%expr t [@pos_right]];
      test_exist "pos_right.s"
        (fun { pos_right = v; _ } -> v)
        [%expr t [@subliner.pos_right]];
      test_exist "non_empty"
        (fun { non_empty = v; _ } -> v)
        [%expr t [@non_empty]];
      test_exist "non_empty.s"
        (fun { non_empty = v; _ } -> v)
        [%expr t [@subliner.non_empty]];
      test_exist "last" (fun { last = v; _ } -> v) [%expr t [@last]];
      test_exist "last.s" (fun { last = v; _ } -> v) [%expr t [@subliner.last]];
      test_exist "default" (fun { default = v; _ } -> v) [%expr t [@default]];
      test_exist "default.s"
        (fun { default = v; _ } -> v)
        [%expr t [@subliner.default]];
    ]
end

module Common = struct
  let test, test_raises = (Term.test, Term.test_raises)

  let test_set =
    [
      test "ignore"
        (fun t -> Term.M.map (fun _ -> assert false) t |> ignore)
        [%expr t [@irrelevant]];
      (* level priority *)
      test "priority_0"
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1))
        [%expr t [@ocaml.doc] [@doc ""]];
      test "priority_1"
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1))
        [%expr t [@doc] [@subliner.doc ""]];
      test "priority_2"
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1))
        [%expr t [@ocaml.doc] [@subliner.doc ""]];
      test "priority_3"
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1))
        [%expr t [@doc] [@doc ""]];
      test "priority_4"
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1))
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
    test_impl (fun expr ->
        M.(parse expr |> map (fun (_loc, structure) -> structure)))

  let test_exist name func expr =
    test ("attr." ^ name) (fun t -> assert (t |> func |> Option.is_some)) expr

  let test_set =
    [
      test "empty"
        (fun t -> M.map (fun _ -> assert false) t |> ignore)
        [%expr t];
      test_exist "deprecated"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@deprecated]];
      test_exist "deprecated.s"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@subliner.deprecated]];
      test_exist "deprecated_"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@deprecated_]];
      test_exist "deprecated_.s"
        (fun { deprecated = v; _ } -> v)
        [%expr t [@subliner.deprecated_]];
      test_exist "man_xrefs"
        (fun { man_xrefs = v; _ } -> v)
        [%expr t [@man_xrefs]];
      test_exist "man_xrefs.s"
        (fun { man_xrefs = v; _ } -> v)
        [%expr t [@subliner.man_xrefs]];
      test_exist "man" (fun { man = v; _ } -> v) [%expr t [@man]];
      test_exist "man.s" (fun { man = v; _ } -> v) [%expr t [@subliner.man]];
      test_exist "envs" (fun { envs = v; _ } -> v) [%expr t [@envs]];
      test_exist "envs.s" (fun { envs = v; _ } -> v) [%expr t [@subliner.envs]];
      test_exist "exits" (fun { exits = v; _ } -> v) [%expr t [@exits]];
      test_exist "exits.s"
        (fun { exits = v; _ } -> v)
        [%expr t [@subliner.exits]];
      test_exist "sdocs" (fun { sdocs = v; _ } -> v) [%expr t [@sdocs]];
      test_exist "sdocs.s"
        (fun { sdocs = v; _ } -> v)
        [%expr t [@subliner.sdocs]];
      test_exist "docs" (fun { docs = v; _ } -> v) [%expr t [@docs]];
      test_exist "docs.s" (fun { docs = v; _ } -> v) [%expr t [@subliner.docs]];
      test_exist "doc" (fun { doc = v; _ } -> v) [%expr t [@doc]];
      test_exist "doc.s" (fun { doc = v; _ } -> v) [%expr t [@subliner.doc]];
      test_exist "ocaml.doc" (fun { doc = v; _ } -> v) [%expr t [@ocaml.doc]];
      test_exist "version" (fun { version = v; _ } -> v) [%expr t [@version]];
      test_exist "version.s"
        (fun { version = v; _ } -> v)
        [%expr t [@subliner.version]];
      test_exist "name" (fun { name = v; _ } -> v) [%expr t [@name]];
      test_exist "name.s" (fun { name = v; _ } -> v) [%expr t [@subliner.name]];
    ]
end
