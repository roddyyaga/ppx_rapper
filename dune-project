(lang dune 2.0)

(name ppx_rapper)

(version 3.1.0)

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
  pg_query
  (ppxlib
   (>= 0.3.1))
  (base
   (>= v0.11.1))
  (caqti
   (>= 1.7.0))))

(package
 (name ppx_rapper_async)
 (synopsis "Async support for ppx_rapper")
 (depends
  (ocaml
   (>= 4.07))
  (ppx_rapper
   (= :version))
  caqti-async
  async))

(package
 (name ppx_rapper_lwt)
 (synopsis "Lwt support for ppx_rapper")
 (depends
  (ocaml
   (>= 4.07))
  (ppx_rapper
   (= :version))
  caqti-lwt
  lwt
  (caqti-type-calendar :with-test)))

(package
 (name ppx_rapper_eio)
 (synopsis "EIO support for ppx_rapper")
 (depends
  (ocaml
   (>= 4.07))
  (ppx_rapper
   (= :version))
  caqti-eio
  eio
  (caqti-type-calendar :with-test)))
