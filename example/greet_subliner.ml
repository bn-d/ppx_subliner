type subparams = { night : bool; name : string } [@@deriving_inline subliner]
let _ = fun (_ : subparams) -> ()
let (subparams_cmdliner_term : unit -> subparams Cmdliner.Term.t) =
  fun () ->
    let make night name = { night; name }
    and subterm_night =
      let info : Cmdliner.Arg.info = Cmdliner.Arg.info ["night"] in
      Cmdliner.Arg.value (Cmdliner.Arg.flag info)
    and subterm_name =
      let info : Cmdliner.Arg.info = Cmdliner.Arg.info ["name"] in
      Cmdliner.Arg.required
        (Cmdliner.Arg.opt (Cmdliner.Arg.some Cmdliner.Arg.string) None info) in
    let open Cmdliner.Term in ((const make) $ subterm_night) $ subterm_name
let _ = subparams_cmdliner_term
[@@@end]

(*let subparams_cmdliner_term () =
  let night_term =
    let doc = "" in
    Cmdliner.Arg.(value & flag & info [ "night" ] ~doc)
  and name_term =
    let doc = "" in
    Cmdliner.Arg.(value & pos 0 string "" & info [] ~doc)
  and _type_transformation night name = { night; name } in
  Cmdliner.Term.(const _type_transformation $ night_term $ name_term)*)

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
