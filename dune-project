(lang dune 3.7)

(name ojson)

(generate_opam_files true)

(source
 (github hitgif/ojson))

(authors "HITGIF")

(maintainers "HITGIF")

(license LICENSE)

(package
 (name ojson)
 (synopsis "JSON parser")
 (depends ocaml dune)
 (tags
  ("json" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
