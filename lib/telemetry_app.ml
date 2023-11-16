open Riot_api

type event = Telemetry.event = ..

let name = "Riot.Telemetry"

module Dispatcher = struct
  type Message.t += Event of Telemetry.event [@@unboxed]

  let __main_dispatcher__ : Pid.t ref = ref Pid.zero

  let rec loop () =
    match receive () with
    | Event e ->
        Telemetry.emit e;
        loop ()
    | _ -> loop ()

  let start_link () =
    let pid = spawn_link (fun () -> loop ()) in
    __main_dispatcher__ := pid;
    Ok pid

  let child_spec () = Supervisor.child_spec ~start_link ()
  let emit ev = send !__main_dispatcher__ (Event ev)
end

let start () = Dispatcher.start_link ()
let emit = Dispatcher.emit
let attach = Telemetry.attach
