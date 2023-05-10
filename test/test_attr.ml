open Ppxlib

let loc = Location.none

let test_impl
    (parser : attributes -> 'a)
    name
    (expression : expression)
    (check : 'a -> unit) =
  let f () = expression.pexp_attributes |> parser |> check in
  Alcotest.test_case name `Quick f

module Term = struct
  module T = Ppx_subliner.Attribute_parser.Term

  let test =
    test_impl (fun expr ->
        T.(parse expr |> map (fun (_loc, structure) -> structure)))

  let test_exist name expr func =
    test ("attr." ^ name) expr (fun t -> assert (t |> func |> Option.is_some))

  let test_set =
    [
      test "empty" [%expr t] (fun t ->
          T.map (fun _ -> assert false) t |> ignore);
      test_exist "deprecated" [%expr t [@deprecated]]
        (fun { deprecated = v; _ } -> v);
      test_exist "deprecated.s" [%expr t [@subliner.deprecated]]
        (fun { deprecated = v; _ } -> v);
      test_exist "deprecated_" [%expr t [@deprecated_]]
        (fun { deprecated = v; _ } -> v);
      test_exist "deprecated_.s" [%expr t [@subliner.deprecated_]]
        (fun { deprecated = v; _ } -> v);
      test_exist "absent" [%expr t [@absent]] (fun { absent = v; _ } -> v);
      test_exist "absent.s" [%expr t [@subliner.absent]]
        (fun { absent = v; _ } -> v);
      test_exist "doc" [%expr t [@doc]] (fun { doc = v; _ } -> v);
      test_exist "doc.s" [%expr t [@subliner.doc]] (fun { doc = v; _ } -> v);
      test_exist "ocaml.doc" [%expr t [@ocaml.doc]] (fun { doc = v; _ } -> v);
      test_exist "docs" [%expr t [@docs]] (fun { docs = v; _ } -> v);
      test_exist "docs.s" [%expr t [@subliner.docs]] (fun { docs = v; _ } -> v);
      test_exist "docv" [%expr t [@docv]] (fun { docv = v; _ } -> v);
      test_exist "docv.s" [%expr t [@subliner.docv]] (fun { docv = v; _ } -> v);
      test_exist "env" [%expr t [@env]] (fun { env = v; _ } -> v);
      test_exist "env.s" [%expr t [@subliner.env]] (fun { env = v; _ } -> v);
      test_exist "names" [%expr t [@name]] (fun { names = v; _ } -> v);
      test_exist "names.s" [%expr t [@subliner.name]] (fun { names = v; _ } ->
          v);
      test_exist "pos" [%expr t [@pos]] (fun { pos = v; _ } -> v);
      test_exist "pos.s" [%expr t [@subliner.pos]] (fun { pos = v; _ } -> v);
      test_exist "pos_all" [%expr t [@pos_all]] (fun { pos_all = v; _ } -> v);
      test_exist "pos_all.s" [%expr t [@subliner.pos_all]]
        (fun { pos_all = v; _ } -> v);
      test_exist "pos_left" [%expr t [@pos_left]] (fun { pos_left = v; _ } -> v);
      test_exist "pos_left.s" [%expr t [@subliner.pos_left]]
        (fun { pos_left = v; _ } -> v);
      test_exist "pos_right" [%expr t [@pos_right]] (fun { pos_right = v; _ } ->
          v);
      test_exist "pos_right.s" [%expr t [@subliner.pos_right]]
        (fun { pos_right = v; _ } -> v);
      test_exist "non_empty" [%expr t [@non_empty]] (fun { non_empty = v; _ } ->
          v);
      test_exist "non_empty.s" [%expr t [@subliner.non_empty]]
        (fun { non_empty = v; _ } -> v);
      test_exist "last" [%expr t [@last]] (fun { last = v; _ } -> v);
      test_exist "last.s" [%expr t [@subliner.last]] (fun { last = v; _ } -> v);
    ]
end

module Common = struct
  module T = Term.T

  let test = Term.test

  let test_raises name (expression : expression) expected =
    Utils.test_raises name expected (fun () ->
        T.parse expression.pexp_attributes)

  let test_set =
    [
      test "ignore" [%expr t [@irrelevant]] (fun t ->
          T.map (fun _ -> assert false) t |> ignore);
      (* level priority *)
      test "priority_0" [%expr t [@ocaml.doc] [@doc ""]] (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1));
      test "priority_1" [%expr t [@doc] [@subliner.doc ""]] (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1));
      test "priority_2" [%expr t [@ocaml.doc] [@subliner.doc ""]]
        (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1));
      test "priority_3" [%expr t [@doc] [@doc ""]] (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1));
      test "priority_4" [%expr t [@subliner.doc ""] [@doc]] (fun { doc; _ } ->
          doc |> Option.get |> fun l -> assert (List.length l = 1));
      (* expected failure *)
      test_raises "invalid_payload" [%expr t [@doc: int]]
        "unsupported payload for attribute";
      test_raises "invalid_attr" [%expr t [@subliner.irrelevant]]
        "unexpected attribute name: irrelevant";
    ]
end
