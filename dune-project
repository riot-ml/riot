(lang dune 3.11)
(using mdx 0.4)

(name riot)

(generate_opam_files true)
(cram enable)

(source
 (github riot-ml/riot))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name riot)
 (synopsis "An actor-model multi-core scheduler for OCaml 5")
 (description
  "Riot is an actor-model multi-core scheduler for OCaml 5. It brings Erlang-style concurrency to the language, where lighweight process communicate via message passing")
 (depends
  (bytestring (= :version))
  (castore (and :with-test (>= "0.0.2")))
  (config (>= "0.0.1"))
  (gluon (= :version))
  (rio (= :version))
  (mdx (and :with-test (>= "2.3.1")))
  (mirage-crypto (>= "0.11.2"))
  (mirage-crypto-rng (>= "0.11.2"))
  (mtime (>= "2.0.0"))
  (ocaml (>= "5.1"))
  (odoc (and :with-doc (>= "2.2.2")))
  (ptime (>= "1.1.0"))
  (randomconv (>= "0.1.3"))
  (telemetry (>= "0.0.1"))
  (tls (>="0.17.3"))
  (uri (>= "4.4.0"))
  (x509 (and :with-test (>= "0.16.5")))
  dune)
 (tags
  (multicore erlang actor "message-passing" processes)))

(package
  (name rio)
  (synopsis "Ergonomic, composable, efficient read/write streams")
  (description
    "Rio implements ergonomic, composable, efficient read/write streams that give you a uniform interface for reading from any source and writing to any sync.")
  (depends
    (cstruct (>= "6.2.0"))
    (ocaml (>= "5.1"))
    dune)
  (tags (multicore io streams read write)))

(package
  (name gluon)
  (synopsis "A minimal, portable, and fast API on top of the operating-system's evented I/O API")
  (depends
    (rio (= :version))
    (bytestring (= :version))
    (libc (>= "0.0.1"))
    (config (>= "0.0.1"))
    (ocaml (>= "5.1"))
    (uri (>= "4.4.0"))
    dune)
  (tags (async io tcp udp multicore)))

(package
  (name bytestring)
  (synopsis "Efficient, immutable, pattern-matchable, UTF friendly byte strings")
  (description "The Bytestring package implements efficient, immutable, pattern-matchable, UTF friendly byte strings backed up by a rope implementation with mutable iterator instances and a transient interface in the spirit of Clojure transient data.")
  (authors 
    "Leandro Ostera <leandro@abstractmachines.dev>"
    "Felipe Oliveira Carvalho <felipekde@gmail.com>")
  (depends
    (rio (= :version))
    (ocaml (>= "5.1.1"))
    (ppxlib (>= 0.31.0))
    (qcheck (and :with-test (>= "0.21.3")))
    (sedlex (>= "3.2"))
    (spices (>= "0.0.2"))
    dune
    )
  (tags (bytes strings "pattern matching" "binary strings" rope transient iterators utf8)))
