open Core.Std
open Async.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind

let _ = return
let _ = ( *>>| )
let _ = ( *>>= )


let alias_default ~dir = Alias.create ~dir "DEFAULT"


let recursive_default_scheme ~dir =
  Scheme.rules [
    Rule.default ~dir [
      Dep.subdirs ~dir *>>= fun subs ->
      Dep.all_unit (
        List.map subs ~f:(fun sub -> Dep.alias (alias_default ~dir:sub)))
    ]
  ]


let makefile_basename = "Makefile.jenga"


let default_all_buildable_and_extract ~dir =
  let makefile = Path.relative ~dir makefile_basename in
  Scheme.rules [
    Rule.default ~dir [
      Dep.buildable_targets ~dir *>>= fun all ->
      let all = List.filter all ~f:(fun r -> r <> makefile) in
      Dep.all_unit (
        List.map all ~f:Dep.path
      )
    ];
    Extract_makefile.extract ~from:(alias_default ~dir) ~makefile;
  ]


let scheme ~dir =
  Scheme.all [
    Coq.scheme ~dir;
    Noweb.scheme ~dir;
    Ocaml.scheme ~dir;
    recursive_default_scheme ~dir;
    default_all_buildable_and_extract ~dir;
  ]


let env = Env.create (fun ~dir -> scheme ~dir)
let setup () = Deferred.return env
