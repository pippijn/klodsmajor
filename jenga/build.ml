open Core.Std
open Jenga_lib.Api
let ( *>>| ) = Dep.map


type target_kind =
  | Library
  | Program
with sexp


type build_info =
  | Name of string
  | Description of string
  | Kind of target_kind
  | OCamlSources of string list
  | OCamlRequires of string list
  | CSources of string list
  | CHeaders of string list
  | CRequires of string list
  | Flags of (string * string list) list
with sexp


type build_info_list = build_info list with sexp


type t = {
  name : string;
  description : string;
  kind : target_kind;
  ocaml_sources : string list;
  ocaml_requires : string list;
  c_sources : string list;
  c_headers : string list;
  c_requires : string list;
  flags : string list StringMap.t;
}

let empty = {
  name = "";
  description = "";
  kind = Program;
  ocaml_sources = [];
  ocaml_requires = [];
  c_sources = [];
  c_headers = [];
  c_requires = [];
  flags = StringMap.empty;
}


let t_of_sexp sx =
  let info = build_info_list_of_sexp sx in
  List.fold_left info ~init:empty
    ~f:fun build -> function
      | Name          name           -> { build with name           }
      | Description   description    -> { build with description    }
      | Kind          kind           -> { build with kind           }
      | OCamlSources  ocaml_sources  -> { build with ocaml_sources  }
      | OCamlRequires ocaml_requires -> { build with ocaml_requires }
      | CSources      c_sources      -> { build with c_sources      }
      | CHeaders      c_headers      -> { build with c_headers      }
      | CRequires     c_requires     -> { build with c_requires     }
      | Flags flags ->
          { build with
            flags = List.fold_left flags ~init:build.flags
                ~f:fun flags (file, flag_list) ->
                  let flag_list =
                    (
                      StringMap.find flags file
                      |> Option.value ~default:[]
                    ) @ flag_list
                  in
                  StringMap.add flags ~key:file ~data:flag_list
          }


let name           { name          ; _ } = name
let kind           { kind          ; _ } = kind
let ocaml_sources  { ocaml_sources ; _ } = ocaml_sources
let ocaml_requires { ocaml_requires; _ } = ocaml_requires


let flags_for { flags; _ } source =
  StringMap.find flags source
  |> Option.value ~default:[]



let scheme ~dir scheme =
  let build = Path.relative ~dir "Build.lsp" in
  Scheme.dep (
    Dep.file_exists build *>>| function
    | false ->
        Scheme.no_rules
    | true ->
        Scheme.contents build
          (fun contents ->
             Sexp.of_string (String.strip contents)
             |> t_of_sexp
             |> scheme)
  )


(* Ignore the build file. Necessary for all schemes that don't use the build
   file, as otherwise they will create cyclic dependencies. *)
let ignore_build =
  Scheme.exclude (fun path -> Path.basename path = "Build.lsp")
