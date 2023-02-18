open Cmdliner

type params = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

let english =
  let doc = "Greet in English" in
  let info = Cmd.info ~doc "english" in
  let f { night; name } = Greet.english ~night name in
  Cmd.v info Term.(const f $ params_cmdliner_term ())

let chinese =
  let doc = "Greet in Chinese" in
  let info = Cmd.info ~doc "chinese" in
  let f { night; name } = Greet.chinese ~night name in
  Cmd.v info Term.(const f $ params_cmdliner_term ())

let programmer =
  let doc = "Hello world!" in
  let info = Cmd.info ~doc "programmer" in
  let f () = Greet.programmer () in
  Cmd.v info Term.(const f $ const ())

let cmd =
  let doc = "Greet in different languages!" in
  let info = Cmd.info ~doc "greet" in
  let default = Term.(ret (const (`Help (`Auto, None)))) in
  Cmd.group ~default info [ english; chinese; programmer ]

let () = exit (Cmd.eval cmd)
