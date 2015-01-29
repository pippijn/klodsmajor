open Core.Std
open Jenga_lib.Api


let needs_escape s =
  let needs_escape = [' '; '\''; '"'; '\\'] in
  String.exists s ~f:(List.mem needs_escape)


let shell_escape s =
  if not (needs_escape s) then
    s
  else
    "'" ^
    String.concat_map s ~f:(
      function
      | '\'' -> "'\\''"
      | c -> String.make 1 c
    ) ^
    "'"



let bash ~dir command_string =
  Action.process ~dir ~prog:"bash" ~args:[
    "-e"; "-u"; "-o"; "pipefail";
    "-c"; command_string
  ]

module Bash : sig

  type t
  val create : prog:string -> args:string list -> target:string option -> t
  val action: dir:Path.t -> t list -> Action.t

end = struct

  type t = string

  let create ~prog ~args ~target =
    let com = String.concat ~sep:" "
      (prog :: List.map args ~f:shell_escape)
    in
    match target with
    | None -> com
    | Some target -> com ^ " > " ^ target

  let action ~dir ts =
    let command_string = String.concat ~sep:"; " ts in
    bash ~dir command_string

end

let bash1 ?target prog args = Bash.create ~prog ~args ~target
