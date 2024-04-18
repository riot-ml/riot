open Riot

let update_inner mutex () =
  let res = Mutex.lock mutex succ in
  match res with
  | Ok () -> Printf.printf "Successfully updated mutex!\n"
  | Error reason -> Printf.printf "Update failed: %s\n" @@ Mutex.pp_err reason

let start () =
  let var = Mutex.create 0 in
  let pids = List.init 16 (fun _ -> spawn (update_inner var)) in
  wait_pids pids;
  let final = Result.get_ok @@ Mutex.get var in
  Printf.printf "final mutex value is: %d\n" final;
  assert (final = 16)

let _main = run start
