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


type build = build_info list with sexp


let flags =
  List.fold_left ~init:[]
    ~f:fun acc -> function
      | Flags flags -> flags @ acc
      | _ -> acc


let flags_for build source =
  List.fold_left (flags build) ~init:[]
    ~f:fun acc -> function
      | (file, flags) when file = source ->
          flags @ acc
      | _ -> acc


let ocaml_requires =
  List.fold_left ~init:[]
    ~f:fun acc -> function
      | OCamlRequires requires -> requires @ acc
      | _ -> acc
