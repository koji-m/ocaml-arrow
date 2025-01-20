(lang dune 3.17)

(name arrow)

(generate_opam_files true)

(source
 (github koji-m/ocaml-arrow))

(authors "Koji Matsumoto <otomustam.ijok@gmail.com>")

(maintainers "Koji Matsumoto <otomustam.ijok@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name arrow)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
    (ocaml (>= 4.13))
    (alcotest :with-test)
    zarith
    flatbuffers
    (cppo (and :build (>= 1.1.0))))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
