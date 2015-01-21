open Core.Std


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


let t_of_sexp sx : t =
  let info = build_info_list_of_sexp sx in
  List.fold_left info ~init:empty
    ~f:fun build -> function
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

      | _ -> assert false


let flags_for { flags; _ } source =
  StringMap.find flags source
  |> Option.value ~default:[]


let ocaml_requires { ocaml_requires; _ } = ocaml_requires
let ocaml_sources { ocaml_sources; _ } = ocaml_sources
