open Core.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind


let target_exists ~dir file =
  Dep.glob_listing (Glob.create ~dir file) *>>| function
  | []   -> false
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

let ocamldep ~dir ~flags source =
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
          | (true , true ) ->
              failwith (
                "Module file names for " ^ dep ^
                " differ only in capitalisation"
              )
          | (false, false) -> return () (* not found *)
          | (false, true ) ->
              let lc = String.uncapitalize dep in
              Dep.path (Path.relative ~dir (lc ^ ".cmi"))
          | (true , false) ->
              let lc = dep in
              Dep.path (Path.relative ~dir (lc ^ ".cmi"))


type builder = {
  (* Compiler program used. *)
  compiler : string;
  (* Source extension (.ml/.mli) *)
  source_ext : string;
  (* The file extensions this command will produce (.cmi, .cmo, etc.). Primary
     target extension first (e.g. .cmx for ocamlopt). *)
  target_exts : string list;
} with sexp

type builder_map = (string list * builder) list with sexp


let ocaml_rules ~dir ~build ~basename (builders : builder list) =
  (* Determine the actual target extensions each builder should produce,
     ensuring each file is only built once. *)
  let builder_map =
    List.fold_left builders ~init:[]
      ~f:fun builder_map builder ->
        (* Actual extensions. *)
        let target_exts =
          List.filter builder.target_exts
            ~f:fun (ext : string) ->
              not (List.exists builder_map
                     ~f:fun (exts, _) -> List.mem exts ext)
        in
        (target_exts, builder) :: builder_map
  in

  let rules =
    List.map builder_map
      ~f:fun (target_exts, builder) ->
        (* $basename.ml *)
        let source = basename ^ builder.source_ext in

        let primary_ext =
          match target_exts with
          | [] ->
              (* If this happens, that means everything built by this builder
                 is also built by other builders, so it has no effect. *)
              failwith "Builder has no primary target extensions"
          | x :: _ -> x
        in

        let flags =
          (* Add -package flag with all ocamlfind requirements. *)
          (match Build.ocaml_requires build with
           | [] -> []
           | requires -> ["-package"; String.concat ~sep:"," requires])
          (* General flags for all OCaml builds. *)
          (* Add user-specified file-specific flags. *)
          @ Build.flags_for build source
        in

        let deps = [
          (* The first dependency is our source file. *)
          Dep.path (Path.relative ~dir source);
          (* Run ocamldep to compute other dependencies. *)
          ocamldep ~dir ~flags source;
        ] in

        (* Add -{bin-,}annot flags after dependency discovery, as ocamldep
           wouldn't like to see those. *)
        let flags =
          flags
          @ ["-annot"; "-bin-annot";
             "-safe-string";
             "-principal";
             "-strict-sequence"]
        in

        Rule.simple
          ~targets:(
            List.map target_exts
              ~f:fun target_ext ->
                Path.relative ~dir (basename ^ target_ext)
          )
          ~deps
          ~action:(
            let tmp_name = basename ^ "." ^ builder.compiler in
            let open ShellUtil in
            Bash.action ~dir (
              [
                bash1 "ocamlfind" (
                  [builder.compiler;
                   "-c"; source;
                   "-o"; tmp_name ^ primary_ext]
                  @ flags
                );
              ] @ (
                List.map target_exts
                  ~f:fun ext ->
                    bash1 "mv" [tmp_name ^ ext; basename ^ ext]
              ) @ (
                List.map builder.target_exts
                  ~f:fun ext ->
                    bash1 "rm" ["-f"; tmp_name ^ ext]
              )
            )
          )
  in

  Scheme.rules rules


let compile_scheme ~dir ~build =
  Scheme.dep (
    Dep.glob_listing (Glob.create ~dir "*.ml") *>>| fun inputs ->

    Scheme.all (
      List.map inputs
        ~f:fun ml ->
          (* File name without ".ml". *)
          let basename =
            let basename = Path.basename ml in
            String.slice basename 0 (String.length basename - 3)
          in

          Scheme.dep (
            let mli = Path.relative ~dir (basename ^ ".mli") in

            Dep.file_exists mli *>>| function
            | false ->
                (* No mli => create cmi from ml. *)
                ocaml_rules ~dir ~build ~basename [
                  { compiler = "ocamlc";   source_ext = ".ml" ; target_exts = [".cmo"; ".cmi"; ".cmt"; ".annot"] };
                  { compiler = "ocamlopt"; source_ext = ".ml" ; target_exts = [".cmx"; ".cmi"; ".cmt"; ".annot"; ".o"] };
                ]
            | true ->
                (* Has mli => create cmi from mli. *)
                ocaml_rules ~dir ~build ~basename [
                  { compiler = "ocamlc";   source_ext = ".ml" ; target_exts = [".cmo"; ".cmt"; ".annot"] };
                  { compiler = "ocamlopt"; source_ext = ".ml" ; target_exts = [".cmx"; ".cmt"; ".annot"; ".o"] };
                  { compiler = "ocamlc";   source_ext = ".mli"; target_exts = [".cmi"; ".cmti"] };
                ]
          )
    )
  )


let scheme ~dir =
  Build.scheme ~dir
    (fun build ->
      match Build.ocaml_sources build with
      | [] ->
          Scheme.no_rules
      | _ ->
          compile_scheme ~dir ~build)
