open Core.Std
open Jenga_lib.Api
let ( *>>| ) = Dep.map


let scheme ~dir =
  Build.ignore_build @@
  Scheme.dep (
    Dep.glob_listing (Glob.create ~dir "*.nw") *>>| fun inputs ->
    let rules =
      List.map inputs
        ~f:fun input ->
          let output =
            let basename = Path.basename input in
            String.slice basename 0 (String.length basename - 3)
          in
          Rule.simple
            ~targets:[Path.relative ~dir output]
            ~deps:[Dep.path input]
            ~action:(Action.process ~dir
                       ~prog:"bash"
                       ~args:["-c"; "notangle -L'# %L \"%F\"%N' -Rml " ^
                                    Path.basename input ^ " > " ^ output])
    in
    Scheme.rules rules
  )
