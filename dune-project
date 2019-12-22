(lang dune 2.0.1)
(name ppx_rapper)
(version 0.9.3)
(source (github roddyyaga/ppx_rapper))
(documentation "https://github.com/roddyyaga/ppx_rapper")
(license MIT)
(authors "Roddy MacSween <github@roddymacsween.co.uk>")
(maintainers "Roddy MacSween <github@roddymacsween.co.uk>")
(generate_opam_files true)
(package
(name ppx_rapper)
(synopsis "Syntax extension for Caqti/PostgreSQL queries")
(depends
(ocaml (>= 4.07))
dune
pg_query
ppxlib
)
)
