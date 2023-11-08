(lang dune 3.10)

(name riot)

(generate_opam_files true)

(source
 (github leostera/riot))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(documentation https://github.com/leostera/riot)

(package
 (name riot)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   (ocaml (>= "5.0"))
   dune
   (ptime (>= "1.1.0"))
   (iomux (>= "0.3"))
   (bigstringaf (>= "0.9.1"))
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
