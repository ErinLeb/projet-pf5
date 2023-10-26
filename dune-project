(lang dune 3.0)

(name pf5)

(generate_opam_files)

(authors
  "Clément Allain"
  "Giovanni Bernardi"
  "Jérémy Ledent"
  "Alexandre Moine"
  "Vincent Padovani"
)
(maintainers
  "clement.allain@inria.fr"
  "gio@irif.fr"
  "jeremy.ledent@irif.fr"
  "alexandre.moine@inria.fr"
  "padovani@irif.fr"
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
