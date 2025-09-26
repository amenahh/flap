(lang dune 3.13)
(using dune_site 0.1)
(using menhir 3.0)
(cram enable)

(generate_opam_files true)

;(source (github username/reponame))
;(authors "Adrien Guatto")
(maintainers "Adrien Guatto <guatto@irif.fr>")
;(license AGPL-3.0-or-later)

(package
 (name flap)
 (synopsis "The flap compiler infrastructure")
 (description "Pedgogical compiler infrastructure")
 (depends (ocaml (>= 5.1.0))
          dune dune-site
          pprint sexplib menhir sedlex ppx_sexp_conv
          yojson pure-html domainslib re patdiff ppx_yojson_conv
          )
 (sites (lib runtime)))