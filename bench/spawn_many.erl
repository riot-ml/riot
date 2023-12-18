-module(reference).
-export([main/1]).

loop(N) ->
  receive
    loop_stop -> ok;
    _ -> loop(N+1)
  end.

spawn_processes(ProcCount) ->
  [ spawn(fun () -> loop(0) end) || _ <- lists:seq(0, ProcCount) ].

do_start(ProcCount) ->
  Pids = spawn_processes(ProcCount),
  [ Pid ! loop_stop || Pid <- Pids ],
  wait_pids(Pids).

wait_pids([]) -> ok;
wait_pids([P|T]=Pids) ->
  case is_process_alive(P) of
    true -> wait_pids(Pids);
    false -> wait_pids(T)
  end.

main(_Args) ->
  ProcCount = 10_000,
  {Time, _} = timer:tc(fun () -> do_start(ProcCount) end),
  io:format("spawned/awaited ~p processes in ~p ms\n", [ProcCount, Time/1_000]).
