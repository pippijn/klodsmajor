open Core.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind


let run_coqdep ~dir source =
  Dep.all_unit [
    Dep.glob_change (Glob.create ~dir "*.v");
    Dep.path source;
  ] *>>= fun () ->
  Dep.action_stdout (
    return @@ Action.process ~dir
      ~prog:"coqdep"
      ~args:["-I"; "."; Path.basename source]
  )


let coqdep ~dir source =
  run_coqdep ~dir source *>>= fun deps ->
  match String.rsplit2 ~on:':' (String.strip deps) with
  | None ->
      failwith "Invalid output from coqdep"
  | Some (_, deps) ->
      Dep.all_unit (
        String.split ~on:' ' deps
        |> List.filter ~f:(String.is_suffix ~suffix:".vo")
        |> List.map ~f:(Path.relative ~dir)
        |> List.map ~f:Dep.path
      )
      (*
      Dep.all_unit @@ List.map deps
        ~f:fun dep ->
          assert (Char.is_uppercase dep.[0]);
          let cmi_uc = dep ^ ".cmi" in
          let cmi_lc = String.uncapitalize cmi_uc in

          let make_dep basename =
            Dep.all_unit @@
            List.map dep_exts ~f:fun ext ->
              Dep.path (Path.relative ~dir (basename ^ ext))
          in

          Dep.both (target_exists ~dir cmi_uc) (target_exists ~dir cmi_lc) *>>= function
          | (true , true ) ->
              failwith (
                "Module file names for " ^ dep ^
                " differ only in capitalisation"
              )
          | (false, false) -> return () (* not found *)
          | (false, true ) ->
              make_dep (String.uncapitalize dep)
          | (true , false) ->
              make_dep dep
                *)


let scheme ~dir =
  Build.ignore_build @@
  Scheme.dep (
    Dep.glob_listing (Glob.create ~dir "*.v") *>>| fun inputs ->
    let rules =
      List.map inputs
        ~f:fun input ->
          let output =
            let basename = Path.basename input in
            String.slice basename 0 (String.length basename - 2)
          in
          Rule.simple
            ~targets:[
              Path.relative ~dir (output ^ ".vo");
            ]
            ~deps:[Dep.path input; coqdep ~dir input]
            ~action:(Action.process ~dir
                       ~prog:"coqc"
                       ~args:["-I"; "."; Path.basename input])
    in
    Scheme.rules rules
  )
