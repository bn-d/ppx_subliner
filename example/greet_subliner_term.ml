type params = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

let greet {night; name} = Greet.english ~night name

[%%subliner.term eval.params <- greet]
[@@name "greet"] [@@version "3.14"]
(** Greet in English *)
