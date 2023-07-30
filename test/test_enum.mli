type t = Enum_1 | Enum_2 [@names [ "overide-name"; "o" ]]
[@@deriving subliner_enum]

val test_set : unit Alcotest.test_case list
