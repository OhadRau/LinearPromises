(lang dune 2.7)
(using menhir 2.0)
(generate_opam_files true)

(source (github ohadrau/LinearPromises))

(name linear-promises)
(package
  (name linear-promises)
  (depends (menhir (>= 2.0))))
