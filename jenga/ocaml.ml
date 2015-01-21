open Core.Std
open Jenga_lib.Api
let return = Dep.return
let ( *>>| ) = Dep.map
let ( *>>= ) = Dep.bind

open Bash


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

  let flags =
    flags
    @ ["-annot"; "-bin-annot"]
  in

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
    ~deps
    ~action:(
      let tmp_name = basename ^ "." ^ compiler in
      Bash.action ~dir (
        [
          bash1 "ocamlfind" ([
              compiler; "-c"; source; "-o"; tmp_name ^ List.hd_exn target_exts
            ] @ flags);
        ] @ (
          List.map target_exts
            ~f:fun ext ->
              bash1 "mv" [tmp_name ^ ext; basename ^ ext]
        ) @ [
          bash1 "rm" ["-f"; tmp_name ^ ".annot"];
          bash1 "rm" ["-f"; tmp_name ^ ".cmi"];
          bash1 "rm" ["-f"; tmp_name ^ ".cmt"];
          (*bash1 "rm" ["-f"; tmp_name ^ ".cmti"];*)
        ]
      )
    )


type builder = {
  (* Compiler program used. *)
  compiler : string;
  (* Source extension (.ml/.mli) *)
  source_ext : string;
  (* The file extensions this command will produce (.cmi, .cmo, etc.). Primary
     target extension first (e.g. .cmx for ocamlopt). *)
  target_exts : string list;
}


let ocaml_rules ~dir ~basename (builders : builder list) =
  (* Determine the actual target extensions each builder should produce,
     ensuring each file is only built once. *)
  let builder_map =
    List.fold_left builders ~init:[]
      ~f:fun builder_map builder ->
        (* Actual extensions. *)
        let actual =
          List.filter builder.target_exts
            ~f:fun (ext : string) ->
              List.exists builder_map
                ~f:fun (exts, _) -> not (List.mem exts ext)
        in
        (actual, builder) :: builder_map
  in

  ignore dir;
  ignore basename;
  ignore builder_map;
  print_endline "heyo";
  Scheme.all []


let ocaml_scheme ~dir ~build =
  let _ = compile in
  ignore build;
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
                ocaml_rules ~dir ~basename [
                  { compiler = "ocamlc";   source_ext = ".ml" ; target_exts = [".cmo"; ".cmi"; ".cmt"; ".annot"] };
                  { compiler = "ocamlopt"; source_ext = ".ml" ; target_exts = [".cmx"; ".cmi"; ".cmt"; ".annot"; ".o"] };
                ]
            | true ->
                (* Has mli => create cmi from mli. *)
                ocaml_rules ~dir ~basename [
                  { compiler = "ocamlc";   source_ext = ".ml" ; target_exts = [".cmo"; ".cmt"; ".annot"] };
                  { compiler = "ocamlopt"; source_ext = ".ml" ; target_exts = [".cmx"; ".cmt"; ".annot"; ".o"] };
                  { compiler = "ocamlc";   source_ext = ".mli"; target_exts = [".cmi"; ".cmti"] };
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
               |> Build.t_of_sexp
             in

             match Build.ocaml_sources build with
             | [] ->
                 Scheme.all []
             | _ ->
                 ocaml_scheme ~dir ~build
          )
  )
