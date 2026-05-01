(lang dune 3.20)

(name oclas)

(generate_opam_files true)

(source
 (github geocaml/ocaml-las))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(maintenance_intent "(latest)")

(license ISC)

(documentation https://github.com/geocaml/ocaml-las)

(package
 (name oclas)
 (synopsis "Library for reading point cloud data")
 (description "A library for reading and working with LAS, LAZ, and COPC based point cloud data.")
 (depends (ocaml (>= 5.1)) dune eio_main (alcotest :with-test) (odoc :with-doc) (ocamlformat (and (>= 0.29.0) :with-dev-setup )))
 (tags
  (GIS las laz "point cloud")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
