type subparams = { night : bool; name : string [@pos 0] }

let subparams_cmdliner_term () =
  let night_term =
    let doc = "" in
    Cmdliner.Arg.(value & flag & info [ "night" ] ~doc)
  and name_term =
    let doc = "" in
    Cmdliner.Arg.(value & pos 0 string "" & info [] ~doc)
  and _type_transformation night name = { night; name } in
  Cmdliner.Term.(const _type_transformation $ night_term $ name_term)

type params =
  | English of subparams  (** Greet in English *)
  | Chinese of subparams  (** Greet in Chinese *)
  | Programmer  (** Hello world! *)
[@@deriving_inline subliner]

let _ = fun (_ : params) -> ()

let (params_cmdliner_group_cmds : (params -> 'a) -> 'a Cmdliner.Cmd.t list) =
 fun func ->
  let subcmd_english =
    let info : Cmdliner.Cmd.info =
      Cmdliner.Cmd.info ~doc:" Greet in English " "english"
    and handle params = func (English params) in
    let open Cmdliner in
    Cmd.v info
      (let open Term in
      const handle $ subparams_cmdliner_term ())
  and subcmd_chinese =
    let info : Cmdliner.Cmd.info =
      Cmdliner.Cmd.info ~doc:" Greet in Chinese " "chinese"
    and handle params = func (Chinese params) in
    let open Cmdliner in
    Cmd.v info
      (let open Term in
      const handle $ subparams_cmdliner_term ())
  and subcmd_programmer =
    let info : Cmdliner.Cmd.info =
      Cmdliner.Cmd.info ~doc:" Hello world! " "programmer"
    and handle () = func Programmer in
    let open Cmdliner in
    Cmd.v info
      (let open Term in
      const handle $ const ())
  in
  [ subcmd_english; subcmd_chinese; subcmd_programmer ]

let _ = params_cmdliner_group_cmds

[@@@end]

let greet = function
  | English { night; name } -> Greet.english ~night name
  | Chinese { night; name } -> Greet.chinese ~night name
  | Programmer -> Greet.programmer ()

[%%subliner.cmds
eval.params <- greet]
[@@name "greet"] [@@version "3.14"]
(** Greet in different languages! *)
