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
let test =
  test_impl (fun expr ->
      Ppx_subliner.Attribute_parser.Term.(
        parse expr |> map (fun (_loc, structure) -> structure)))

        let test_exist name expr func =
          test name expr (fun t -> assert (t |> func |> Option.is_some))
let test_set =
  [
    (** basic *)
    test_exist "deprecated" [%expr t [@deprecated]]
      (fun { deprecated = v; _ } -> v);
    test_exist "deprecated.s" [%expr t [@subliner.deprecated]]
      (fun { deprecated = v; _ } -> v);
    test_exist "deprecated_" [%expr t [@deprecated_]]
      (fun { deprecated = v; _ } -> v);
    test_exist "deprecated_.s" [%expr t [@subliner.deprecated_]]
      (fun { deprecated = v; _ } -> v);
    test_exist "absent" [%expr t [@absent]] (fun { absent = v; _ } -> v);
    test_exist "absent.s" [%expr t [@subliner.absent]] (fun { absent = v; _ } ->
        v);
    test_exist "doc" [%expr t [@doc]] (fun { doc = v; _ } -> v);
    test_exist "doc.s" [%expr t [@subliner.doc]] (fun { doc = v; _ } -> v);
    test_exist "ocaml.doc" [%expr t [@ocaml.doc]] (fun { doc = v; _ } -> v);
    test_exist "docs" [%expr t [@docs]] (fun { docs = v; _ } -> v);
    test_exist "docs.s" [%expr t [@subliner.docs]] (fun { docs = v; _ } -> v);
    test_exist "docv" [%expr t [@docv]] (fun { docv = v; _ } -> v);
    test_exist "docv.s" [%expr t [@subliner.docv]] (fun { docv = v; _ } -> v);
    test_exist "env" [%expr t [@env]] (fun { env = v; _ } -> v);
    test_exist "env.s" [%expr t [@subliner.env]] (fun { env = v; _ } -> v);
    test_exist "name" [%expr t [@name]] (fun { name = v; _ } -> v);
    test_exist "name.s" [%expr t [@subliner.name]] (fun { name = v; _ } -> v);
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

end