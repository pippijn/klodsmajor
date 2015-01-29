open Core.Std
open Jenga_lib.Api
let ( *>>| ) = Dep.map

type action = Reflected.Action.t

let sexp_of_action _ = Sexp.Atom "<action>"

type trip = Reflected.Trip.t = {
  targets : Path.t list;
  deps : Path.t list;
  action : action;
} with sexp_of
type trips = trip list with sexp_of


let strip_ext path =
  let basename = Path.basename path in
  match String.rsplit2 basename ~on:'.' with
  | None -> basename
  | Some (basename, _) -> basename


let make_graph ~trips =
  let open Reflected.Trip in

  let rec collect graph = function
    | [] -> graph
    | trip :: trips ->
        let graph =
          let target =
            match trip.targets with
            | [] -> failwith "Inconsistency: trip without targets"
            | target :: _ -> strip_ext target
          in
          let deps = List.map trip.deps ~f:strip_ext in
          assert (not @@ List.Assoc.mem graph target);
          List.Assoc.add graph target deps
        in

        collect graph trips
  in

  collect [] trips


(* remove self dependency *)
let remove_self_dep graph =
  List.map graph ~f:fun (target, deps) ->
    (target, List.filter deps ~f:fun d -> d <> target)


let get_deps ~graph lib =
  match List.Assoc.find graph lib with
  | None -> []
  | Some deps -> deps


let rec dfs ~graph acc later todo progress =
  match todo, later with
  | [], [] ->
      List.rev acc
  | [], later ->
      if progress then
        dfs ~graph acc [] later false
      else
        invalid_arg "un-orderable data"
  | x :: todo, later ->
      let deps = get_deps ~graph x in
      if List.for_all ~f:(fun dep -> List.mem acc dep) deps then
        dfs ~graph (x::acc) later todo true
      else
        dfs ~graph acc (x::later) todo progress


let topological_sort ~suffixes paths =
  let keep path =
    List.exists suffixes ~f:fun suffix ->
      String.is_suffix ~suffix (Path.basename path)
  in

  Reflect.reachable paths ~keep *>>| fun trips ->

  (*print_endline (Sexp.to_string_hum (sexp_of_trips trips));*)

  let graph =
    make_graph ~trips
    |> remove_self_dep
  in

  let starts, todo =
    (* list items, each being unique *)
    let unique_items =
      List.map graph ~f:(fun (lib, deps) -> lib :: deps) |>
      List.concat |>
      List.dedup
    in
    List.partition_tf ~f:(fun lib -> get_deps ~graph lib = []) unique_items
  in
  dfs ~graph starts [] todo false

  |> List.map
    ~f:(fun name ->
        let trip =
          List.find_exn trips ~f:fun { targets; _ } ->
            List.exists targets ~f:fun target ->
              strip_ext target = name
        in
        List.filter trip.targets ~f:keep)
  |> List.concat
