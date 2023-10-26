(lang dune 3.0)

(name pf5)

(generate_opam_files)

(authors
  "Clément Allain"
  "Alexandre Moine"
)
(maintainers
  "clement.allain@inria.fr"
)

(package
  (name pf5)
  (synopsis "Projet PF5")
  (depends
    ocaml
    utop
    fmt
    alcotest
  )
)
