type subparams = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

type params =
  | English of subparams  (** Greet in English *)
  | Chinese of subparams  (** Greet in Chinese *)
[@@deriving subliner]

let greet = function
  | English { night; name } -> Greet.english ~night name |> Result.ok
  | Chinese { night; name } -> Greet.chinese ~night name |> Result.ok

[%%subliner.cmds
eval_result.params <- greet]
[@@version "3.14"]
(** Greet in different languages! *)
