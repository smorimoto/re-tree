(lang dune 3.17)
(name re-tree)

(license ISC)
(maintainers "Sora Morimoto <sora@morimoto.io>")
(authors "Sora Morimoto <sora@morimoto.io>")
(source (github smorimoto/re-tree))

(generate_opam_files true)

(package
 (name re-tree)
 (synopsis "A minimal Unix tree command implementation written in pure Native Reason")
 (description "A minimal Unix tree command implementation written in pure Native Reason.")
 (depends
  reason
  (ocaml-lsp-server :with-dev-setup)))
