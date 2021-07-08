(lang dune 2.9)

(name omditor)

(using mdx 0.1)

(source
 (github patricoferris/omditor))

(license MIT)

(authors "Patrick Ferris")

(maintainers "pf341@patricoferris.com")

(generate_opam_files true)

(package
 (name omditor)
 (synopsis "OCaml Markdown Editor")
 (description "A markdown client-server editor built with Irmin")
 (depends 
   brr
   dream 
   js_of_ocaml
   js_of_ocaml-lwt 
   omd
   crunch
   cohttp-lwt-unix
   cohttp-lwt-jsoo
   irmin 
   irmin-unix
   irmin-http
   irmin-indexeddb
   irmin-git))

