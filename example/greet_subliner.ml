open Cmdliner

type subparams = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

type params =
  | English of subparams  (** Greet in English *)
  | Chinese of subparams  (** Greet in Chinese *)
[@@deriving_inline subliner]
let _ = fun (_ : params) -> ()
let (make_params_cmdliner_group_cmds :
  (params -> 'a) -> 'a Cmdliner.Cmd.t list) =
  fun func ->
    let subcmd_english =
      let info = Cmdliner.Cmd.info "english"
      and f params = func (English params) in
      let open Cmdliner in
        Cmd.v info
          (let open Term in (const f) $ (subparams_cmdliner_term ()))
    and subcmd_chinese =
      let info = Cmdliner.Cmd.info "chinese"
      and f params = func (Chinese params) in
      let open Cmdliner in
        Cmd.v info
          (let open Term in (const f) $ (subparams_cmdliner_term ())) in
    [subcmd_english; subcmd_chinese]
let _ = make_params_cmdliner_group_cmds
[@@@end]

(** Greet in different languages! *)
let (greet [@deriving subliner] [@version "3.14"]) = function
  | English { night; name } -> Greet.english ~night name
  | Chinese { night; name } -> Greet.chinese ~night name

let eval_params func =
  let cmd =
    let info =
      let doc = "Greet in different languages!" in
      Cmd.info ~doc "greet"
    in
    let default = Term.(ret (const (`Help (`Auto, None)))) in
    let group_cmd = make_params_cmdliner_group_cmds func in
    Cmd.group ~default info group_cmd
  in
  exit (Cmd.eval cmd)

let () = eval_params greet
