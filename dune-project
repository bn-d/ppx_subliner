(lang dune 2.7)
(name ppx_subliner)
(version "0.1.1")

(generate_opam_files true)

(source (github bn-d/ppx_subliner))
(license MIT)
(authors "Boning <me@boni.ng>")
(maintainers "Boning <me@boni.ng>")
(documentation "https://boni.ng/ppx_subliner/ppx_subliner/index.html")

(package
 (name ppx_subliner)
 (synopsis "[@@deriving subliner]/[%%subliner]")
 (description "Ppxlib based [@@deriving] plugin to generate Cmdliner group \
sub-commands and Ppxlib based rewriter to generate Cmdliner evaluation for \
command line tools.")
 (depends
  (ppxlib (>= 0.10.0))
  (cmdliner (>= 1.1.0))
  (alcotest :with-test)
  (ppx_deriving_cmdliner :with-test)))
