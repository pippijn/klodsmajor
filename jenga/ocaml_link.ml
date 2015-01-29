open Core.Std
open Jenga_lib.Api
(*let return = Dep.return*)
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind


let link_rule ~dir ~deps ~linker ~flags ~objects ~target =
  Rule.simple
    ~targets:[Path.relative ~dir @@ target]
    ~deps
    ~action:(
      Action.process ~dir ~prog:"ocamlfind"
        ~args:(
          [linker; "-o"; target]
          @ flags
          @ List.map objects ~f:Path.basename
        )
    )


let link_program ~dir ~build =
  let flags =
    ["-thread"]
    (* Add -package flag with all ocamlfind requirements. *)
    @ match Build.ocaml_requires build with
    | [] -> []
    | requires ->
        ["-package"; String.concat ~sep:"," requires; "-linkpkg"]
  in

  Scheme.all (
    List.map [
      "ocamlc", "byte", [".cmo"];
      "ocamlopt", "native", [".cmx"; ".o"];
    ] ~f:fun (linker, output, suffixes) ->
      Scheme.dep (
        Dep.buildable_targets ~dir *>>=
        BuildOrder.topological_sort ~suffixes *>>| fun inputs ->

        let suffix = List.hd_exn suffixes in

        let objects =
          List.filter inputs ~f:fun path ->
            String.is_suffix ~suffix (Path.basename path)
        in

        let deps = List.map inputs ~f:Dep.path in

        let target = Build.name build ^ "." ^ output in

        Scheme.rules [
          link_rule ~dir ~deps ~linker ~flags ~objects ~target
        ]
      )
  )


let link_library ~dir ~build =
  ignore dir;
  ignore build;
  Scheme.no_rules


let scheme ~dir =
  Build.scheme ~dir
    (fun build ->
      match Build.ocaml_sources build with
      | [] ->
          Scheme.no_rules
      | _ ->
          match Build.kind build with
          | Build.Program ->
              link_program ~dir ~build
          | Build.Library ->
              link_library ~dir ~build)
