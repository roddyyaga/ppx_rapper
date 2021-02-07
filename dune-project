(lang dune 2.0.1)

(name ppx_rapper)

(version 2.0.0)

(source
 (github roddyyaga/ppx_rapper))

(documentation "https://github.com/roddyyaga/ppx_rapper")

(license MIT)

(authors "Roddy MacSween <github@roddymacsween.co.uk>")

(maintainers "Roddy MacSween <github@roddymacsween.co.uk>")

(generate_opam_files true)

(package
 (name ppx_rapper)
 (synopsis "Syntax extension for Caqti/PostgreSQL queries")
 (depends
  (ocaml
   (>= 4.07))
  (dune
   (>= 2.0.1))
  pg_query
  ppxlib
  base
  caqti
  caqti-lwt
  (caqti-type-calendar :with-test)))

(package
 (name ppx_rapper_async)
 (synopsis "Async support for ppx_rapper")
 (depends
  (ocaml
   (>= 4.07))
  (dune
   (>= 2.0.1))
  ppx_rapper
  caqti-async
  caqti
  async))

(package
 (name ppx_rapper_lwt)
 (synopsis "Lwt support for ppx_rapper")
 (depends
  (ocaml
   (>= 4.07))
  (dune
   (>= 2.0.1))
  ppx_rapper
  caqti-lwt
  caqti
  lwt))
