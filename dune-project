(lang dune 1.10)
(generate_opam_files true)

(name pp_loc)
(version 1.0.0)
(source (github Armael/pp_loc))
(authors "Armaël Guéneau <armael.gueneau@inria.fr>")
(maintainers "Armaël Guéneau <armael.gueneau@inria.fr>, Steffen Smolka <smolka@cs.cornell.edu>")
(documentation "https://Armael.github.io/pp_loc/pp_loc/")

(package
  (name pp_loc)
  (depends
    (ocaml (>= 4.02.3))
    (dune (and :build (>= 1.10)))
    (stdlib-shims (<> 0))
    (odoc :with-doc)
  )
  (synopsis "Quote and highlight input fragments at a given source location")
  (description "\
Decent error reporting (for example, in a parser, a compiler, ...) typically involves collecting locations, in order to indicate to the user the position of an error in the source file.
This library provides support for quoting and highlighting the input fragment that corresponds to a given source location (or set of source locations).
"))
