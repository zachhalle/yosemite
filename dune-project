(lang dune 3.4)

(name yosemite)

(generate_opam_files true)

(source
 (github zachhalle/yosemite))

(authors "Zach Halle")

(maintainers "Zach Halle")

(license LICENSE)

(documentation https://github.com/zachhalle/yosemite)

(package
 (name yosemite)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
