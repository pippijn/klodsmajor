open Core.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind
let ( *>>| ) = Dep.map

module F(X : sig
           val dir : Path.t
         end) = struct
  open X

  let keep _path = true

  let wrap_relative_cd_from path s =
    if dir = path then s else
      let relative_cd = Path.reach_from ~dir path in
      "(cd "^relative_cd^"; "^s^")"

  let format_path = Path.reach_from ~dir
  let format_paths xs = String.concat ~sep:" " (List.map xs ~f:format_path)

  let format_trip {Reflected.Trip.deps;targets;action} =
    let deps = List.filter deps ~f:keep in
    let targets = List.filter targets ~f:keep in
    let action_string =
      wrap_relative_cd_from (Reflected.Action.dir action)
        (Reflected.Action.string_for_one_line_make_recipe action)
    in
    match targets with
    | [] -> "#rule with no targets!"
    | t1::rest ->
        let touch_string =
          match rest with | [] -> "" | _ ->
            "\n\ttouch "^
            (String.concat ~sep:" " (List.map rest ~f:format_path))
        in
        String.concat (List.map rest ~f:(fun target ->
            (format_path target)^" : "^(format_path t1)^"\n"
          )) ^ (format_path t1)^" : "^(format_paths deps)^"\n\t"
        ^action_string ^touch_string

  let format_makefile ~roots trips =
    "\nall : "^(format_paths roots)^"\n\n"^(String.concat ~sep:"\n\n" (List.map trips ~f:format_trip))^"\n"

  let extract ~makefile ~from:alias =
    Rule.create ~targets:[makefile] (
      Reflect.alias alias *>>= fun roots ->
      Reflect.reachable ~keep roots *>>| fun trips ->
      Action.save (format_makefile ~roots trips) ~target:makefile
    );

end

let extract ~makefile ~from =
  let dir = Path.dirname makefile in
  let module M = F (struct let dir = dir end) in
  M.extract ~makefile ~from
