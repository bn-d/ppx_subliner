open Cmdliner

type subparams = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

(** Greet in different languages! *)
type params =
  | English of subparams  (** Greet in English *)
  | Chinese of subparams  (** Greet in Chinese *)
(*[@@deriving subliner]*)

let handle = function
  | English { night; name } -> Greet.english ~night name
  | Chinese { night; name } -> Greet.chinese ~night name

let eval_params handle =
  let subcmd_english =
    let doc = "Greet in English" in
    let info = Cmd.info ~doc "english" in
    let f p = handle (English p) in
    Cmd.v info Term.(const f $ subparams_cmdliner_term ())
  and subcmd_chinese =
    let doc = "Greet in Chinese" in
    let info = Cmd.info ~doc "chinese" in
    let f p = handle (Chinese p) in
    Cmd.v info Term.(const f $ subparams_cmdliner_term ())
  in
  let cmd =
    let doc = "Greet in different languages!" in
    let info = Cmd.info ~doc "greet" in
    let default = Term.(ret (const (`Help (`Auto, None)))) in
    Cmd.group ~default info [ subcmd_english; subcmd_chinese ]
  in
  exit (Cmd.eval cmd)

let () = eval_params handle
