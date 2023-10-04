-module(reference).
-export([start/1]).

loop(N) ->
  receive
    loop_stop -> io:format("~p stopped.\n", [self()]);
    _ -> loop(N+1)
  end.

spawn_processes(ProcCount) ->
  [ spawn(fun () -> loop(0) end) || _ <- lists:seq(0, ProcCount) ].

do_start(ProcCount) ->
  io:format("Started\n"),
  {Time, Pids} = timer:tc(fun () -> spawn_processes(ProcCount) end),
  io:format("spawned 1000 processes in ~p\n", [Time]),
  {Time2, _} = timer:tc(fun () -> [ Pid ! loop_stop || Pid <- Pids ] end),
  io:format("sent messages in ~p\n" ,[Time2]).

start(ProcCount) ->
  {Time, _} = timer:tc(fun () -> do_start(ProcCount) end),
  io:format("whole flow took ~p ms\n", [Time/1_000]).



