(lang dune 1.11)
(name pd)

(generate_opam_files true)
(source (github nvgrw/fyp-code))
(license PROPRIETARY)
(authors "Niklas Vangerow")
(maintainers "n@vangerow.net")
(using menhir 2.0)

(package
 (name pd)
 (synopsis "Probabilistic Dragon Compiler")
 (description "Probabilistic Dragon Compiler for Imperial College London Final Year Project")
 (depends
  (ocaml (= 4.07.1))
  (core (= v0.12.4))
  (ppx_deriving (= 4.4))
  (menhir (>= 2.0))
  (ppx_import (>= 1.6.2))))
