(lang dune 2.7)
(generate_opam_files true)

(name pp_loc)
(version 1.0.0)
(source (github Armael/pp_loc))
(authors "Armaël Guéneau <armael.gueneau@ens-lyon.org>")
(maintainers "Armaël Guéneau <armael.gueneau@ens-lyon.org>, Steffen Smolka <smolka@cs.cornell.edu>")
(documentation "https://Armael.github.io/pp_loc/pp_loc/")
(license MIT)

(package
  (name pp_loc)
  (depends
    (ocaml (>= 4.08.0))
    dune
    (odoc :with-doc)
  )
  (synopsis "Quote and highlight input fragments at a given source location")
  (description "\
Decent error reporting (for example, in a parser, a compiler, ...) typically involves collecting locations, in order to indicate to the user the position of an error in the source file.
This library provides support for quoting and highlighting the input fragment that corresponds to a given source location (or set of source locations).
"))
