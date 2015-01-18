open Core.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind


let target_exists ~dir file =
  Dep.glob_listing (Glob.create ~dir file) *>>| function
  | [] -> false
  | _::_ -> true


let run_ocamldep ~dir ~flags source =
  Dep.all_unit [
    Dep.glob_change (Glob.create ~dir "*.ml");
    Dep.glob_change (Glob.create ~dir "*.mli");
    Dep.path (Path.relative ~dir source);
  ] *>>= fun () ->
  Dep.action_stdout (
    return @@ Action.process ~dir
      ~prog:"ocamlfind"
      ~args:(["ocamldep"; "-modules"; source] @ flags)
  )

let ocamldep ~dir ~flags ~dep_exts source =
  run_ocamldep ~dir ~flags source *>>= fun deps ->
  match String.split ~on:' ' (String.strip deps) with
  | [] ->
      failwith "Invalid output from ocamldep"
  | _ :: deps ->
      Dep.all_unit @@ List.map deps
        ~f:fun dep ->
          let cmi_uc = dep ^ ".cmi" in
          let cmi_lc = String.uncapitalize cmi_uc in

          Dep.both (target_exists ~dir cmi_uc) (target_exists ~dir cmi_lc) *>>= function
          | (true, true) -> failwith "Ambiguous"
          | (false, false) -> return () (* not found *)
          | (false, true) ->
              let lc = String.uncapitalize dep in
              Dep.all_unit (
                List.map dep_exts
                  ~f:fun output ->
                    Dep.path (Path.relative ~dir (lc ^ output))
              )
          | (true, false) ->
              let lc = dep in
              Dep.all_unit (
                List.map dep_exts
                  ~f:fun output ->
                    Dep.path (Path.relative ~dir (lc ^ output))
              )


let compile ~dir ~compiler ~build ~dep_exts ~target_exts ~basename source_ext =
  let source = basename ^ source_ext in

  let flags =
    (match Build.ocaml_requires build with
     | [] -> []
     | requires -> ["-package"; String.concat ~sep:"," requires])
    @ Build.flags_for build source
  in

  let deps = [
    Dep.path (Path.relative ~dir source);
    ocamldep ~dir ~flags ~dep_exts:(dep_exts @ target_exts) source;
  ] in

  let deps =
    (if List.mem target_exts ".cmi" then
       []
     else if List.mem target_exts ".cmo" then
       [Dep.path (Path.relative ~dir (basename ^ ".cmi"))]
     else
       [Dep.path (Path.relative ~dir (basename ^ ".cmo"))]
    ) @ deps
  in

  Rule.simple
    ~targets:(
      List.map target_exts
        ~f:fun target_ext ->
          Path.relative ~dir (basename ^ target_ext)
    )
    ~deps:(Dep.path (Path.absolute
                       "/srv/pippijn/code/git/github/tools/ocamlcomp") :: deps)
    ~action:(
      Action.process ~dir
        ~prog:"/srv/pippijn/code/git/github/tools/ocamlcomp"
        ~args:(["ocamlfind"; compiler; "-c"; source] @ flags)
    )


let ocaml_scheme ~dir ~build =
  Scheme.dep (
    Dep.glob_listing (Glob.create ~dir "*.ml") *>>| fun inputs ->

    Scheme.all (
      List.map inputs
        ~f:fun ml ->
          let basename =
            let basename = Path.basename ml in
            String.slice basename 0 (String.length basename - 3)
          in

          let ocamlc   = compile ~dir ~build ~basename ~compiler:"ocamlc" in
          let ocamlopt = compile ~dir ~build ~basename ~compiler:"ocamlopt" in

          Scheme.dep (
            let mli = Path.relative ~dir (basename ^ ".mli") in

            Dep.file_exists mli *>>| function
            | false ->
                (* No mli => create cmi from ml. *)
                Scheme.rules [
                  ocamlc ~dep_exts:[] ~target_exts:[".cmi"; ".cmo"] ".ml";
                  ocamlopt ~dep_exts:[".cmi"; ".cmo"] ~target_exts:[".cmx"; ".o"] ".ml";
                ]
            | true ->
                (* Has mli => create cmi from mli. *)
                Scheme.rules [
                  ocamlc ~dep_exts:[".cmi"] ~target_exts:[".cmo"] ".ml";
                  ocamlopt ~dep_exts:[".cmi"; ".cmo"] ~target_exts:[".cmx"; ".o"] ".ml";
                  ocamlc ~dep_exts:[] ~target_exts:[".cmi"] ".mli";
                ]
          )
    )
  )


let scheme ~dir =
  let build = Path.relative ~dir "Build.lsp" in
  Scheme.dep (
    Dep.file_exists build *>>| function
    | false ->
        Scheme.all []
    | true ->
        Scheme.contents build
          (fun contents ->
             let build =
               Sexplib.Sexp.of_string (String.strip contents)
               |> Build.build_of_sexp
             in

             match
               List.find build
                 ~f:function
                   | Build.Kind _ -> true
                   | _ -> false
             with
             | None ->
                 Scheme.all []
             | Some _ ->
                 ocaml_scheme ~dir ~build
          )
  )
