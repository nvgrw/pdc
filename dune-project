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
  (core (> v0.13.0))))
