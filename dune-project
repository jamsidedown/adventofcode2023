(lang dune 3.11)

(name aoc23)

(generate_opam_files true)

(source
 (github jamsidedown/adventofcode2023))

(authors "Rob Anderson <opensource@robanderson.dev>")

(maintainers "Rob Anderson <opensource@robanderson.dev>")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name aoc23)
 (synopsis "OCaml solutions to the 2023 Advent of Code")
; (description "A longer description")
 (depends
  (ocaml (>= 4.05))
  dune
  (ounit2 :with-test))
 (tags
  ("Advent of code 2023")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
