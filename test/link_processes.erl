-module(link_processes).
-export([main/1]).

loop() -> receive _ -> ok end.

wait_pids([]) -> ok;
wait_pids([P|T]=Pids) ->
  case is_process_alive(P) of
    true -> wait_pids(Pids);
    false -> wait_pids(T)
  end.

main(_Args) ->
  Pid1 = spawn (fun () -> loop() end),
  Pid2 = spawn (fun () -> link(Pid1), loop() end),
  Pid1 ! exit,
  wait_pids([ Pid1, Pid2 ]),
  io:format("linked processes terminated\n").
