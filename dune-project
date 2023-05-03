(lang dune 3.6)

(name lindenaml)

(generate_opam_files true)

(source
 (github dylandronnier/lidenaml))

(authors "Dylan Dronnier")

(license GPLv3)

(package
 (name lindenaml)
 (synopsis "A small command-line utility to generate svg file from L-system")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
