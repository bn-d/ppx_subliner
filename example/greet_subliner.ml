type subparams = { night : bool; name : string [@pos 0] [@docv "NAME"] }
[@@deriving subliner]

type params =
  | English of subparams  (** Greet in English *)
  | Chinese of subparams  (** Greet in Chinese *)
  | Programmer  (** Hello world! *)
[@@deriving subliner]

let greet = function
  | English { night; name } -> Greet.english ~night name
  | Chinese { night; name } -> Greet.chinese ~night name
  | Programmer -> Greet.programmer ()

[%%subliner.cmds
eval.params <- greet]
[@@name "greet"] [@@version "3.14"]
(** Greet in different languages! *)
