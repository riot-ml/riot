open Riot

module Counter = struct
  let rec loop n =
    Logs.log (fun f -> f "%a: %d" Pid.pp (self ()) n);
    if n = 0 then Logs.log (fun f -> f "%a: finished %d" Pid.pp (self ()) n)
    else (
      yield ();
      loop (n - 1))

  let start_link n =
    let pid = spawn (fun () -> loop n) in
    link pid;
    Ok pid
end

module Printer = struct
  let rec loop n =
    if n mod 1000 = 0 then Logs.log (fun f -> f "%a: %d" Pid.pp (self ()) n);
    yield ();
    loop (n + 1)

  let start_link n =
    let pid = spawn (fun () -> loop n) in
    link pid;
    Ok pid
end

let main () =
  let _sup =
    Supervisor.start_link
      ~child_specs:
        [
          Supervisor.child_spec ~start_link:Printer.start_link 0;
          Supervisor.child_spec ~start_link:Counter.start_link 100;
        ]
      ()
  in
  receive () |> ignore;
  ()

let () =
  Logs.set_log_level (Some Info);
  Riot.run @@ main
