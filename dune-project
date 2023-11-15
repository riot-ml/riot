(lang dune 3.10)

(name riot)

(generate_opam_files true)

(source
 (github leostera/riot))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name riot)
 (synopsis "An actor-model multi-core scheduler for OCaml 5")
 (description "Riot is an actor-model multi-core scheduler for OCaml 5. It
              brings Erlang-style concurrency to the language, where
              lighweight process communicate via message passing")
 (depends
   (ocaml (>= "5.1"))
   dune
   (ptime (>= "1.1.0"))
   (iomux (>= "0.3"))
   (bigstringaf (>= "0.9.1"))
   (uri (>= "4.4.0"))
   (odoc (and :with-doc (>= "2.2.2")))
 )
 (tags
  (topics multicore erlang actor "message-passing" processes)))
