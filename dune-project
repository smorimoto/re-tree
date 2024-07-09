(lang dune 3.16)

(name re-tree)

(license ISC)

(authors "Sora Morimoto <sora@morimoto.io>")

(maintainers "Sora Morimoto <sora@morimoto.io>")

(source
 (github smorimoto/re-tree))

(generate_opam_files true)

(package
 (name re-tree)
 (synopsis
  "A minimal Unix tree command implementation written in pure Native Reason")
 (description
  "A minimal Unix tree command implementation written in pure Native Reason.")
 (depends
  ocaml
  reason
  (ocaml-lsp-server :with-dev-setup)))
