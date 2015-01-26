open Jenga_lib.Api


let scheme ~dir =
  Scheme.all [
    Ocaml_compile.scheme ~dir;
    Ocaml_link.scheme ~dir;
  ]
