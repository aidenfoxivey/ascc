(lang dune 3.8)
(using ctypes 0.1)

(name ascc)

(generate_opam_files true)

(source
 (github aidenfoxivey/ascc))

(authors "Aiden Fox Ivey")

(maintainers "Aiden Fox Ivey")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name ascc)
 (synopsis "Aiden's Small C Compiler")
 (description "Baby's first C compiler project.")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
