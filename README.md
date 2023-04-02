# [@@deriving subliner] and [%%subliner]
[![OCaml][ocaml-badge]](#)
[![CI][ci-badge]](https://github.com/bn-d/ppx_subliner/actions/workflows/build.yml)
[![GitHub release status][release-badge]](https://github.com/bn-d/ppx_subliner/releases)
[![Coverage][coveralls-badge]](#)

[ocaml-badge]: https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=white
[ci-badge]: https://github.com/bn-d/ppx_subliner/actions/workflows/build.yml/badge.svg?branch=master
[release-badge]: https://img.shields.io/github/v/release/bn-d/ppx_subliner
[coveralls-badge]: https://img.shields.io/coveralls/bn-d/ppx_subline

`[@@deriving]` plugin to generate [Cmdliner](cmdliner) sub-command groups, and ppx rewriter to generate [Cmdliner](cmdliner) evaluations.

## Installation

`ppx_subliner` can be installed via [OCaml Package Manager](https://opam.ocaml.org/packages/ppx_subliner/).

```console
$ opam install ppx_subliner
```

## Usage
Please see the [documentation](https://boni.ng/ppx_subliner/ppx_subliner/index.html).

[cmdliner]: https://github.com/dbuenzli/cmdliner

## Example

```ocaml
type subparams = { night : bool; name : string [@pos 0] } [@@deriving cmdliner]

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
```
