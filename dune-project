(lang dune 2.7)
(name ppx_subliner)
(version "0.1.2")

(generate_opam_files true)

(source (github bn-d/ppx_subliner))
(license MIT)
(authors "Boning <me@boni.ng>")
(maintainers "Boning <me@boni.ng>")
(documentation "https://boni.ng/ppx_subliner/ppx_subliner/index.html")

(package
 (name ppx_subliner)
 (synopsis "[@@deriving subliner] and [%%subliner] for Cmdliner")
 (description "[@@deriving] plugin to generate Cmdliner sub-command groups \
 and ppx rewriter to generate Cmdliner evaluations.")
 (depends
  (ppxlib (>= 0.10.0))
  (cmdliner (>= 1.1.0))
  (alcotest :with-test)
  (ppx_deriving_cmdliner :with-test)))
