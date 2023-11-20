# `1-hello-world`

A basic project that spins up 1 process and logs a message from it.

Normally, Riot programs will being by opening up the `Riot` module, which
exposes a lot of common functions to work with Processes, sending messages, and
receiving message.

```ocaml
open Riot
```

Every Riot program begins with a call to `Riot.run`. `Riot.run` takes a single
function as input, and _does not terminate_. This is because Riot programs are
expected to _run forever_.

If you want to terminate the Riot runtime, you can call `Riot.shutdown ()` from
anywhere in the program. Keep in mind that this will not await for all
processes to terminate. We will cover graceful-shutdowns of applications later
in this tutorial.

```ocaml
let () = Riot.run @@ fun () ->
```

A Logger is included with Riot that is multi-core friendly, configurable at a
global and local level, and _non-blocking_. This means we can use this logger
from anywhere in the application, and always get messages in a reasonable
order, printed in a readable way (none of those pesky write-conflicts).

```ocaml
  Logger.start () |> Result.get_ok;
```

Riot has a `spawn` function that can be used to create a new process. Riot
processes are _cheap_, and Riot programs can have millions of processes. They
are not like Operating System processes (or threads), and are closer to
green-threads or fibers.

`spawn` takes a `unit -> unit` function as an input, and will give us back a
_pid_. A Pid is a Process Identifier. Pids are unique during the execution of a
program and can be used to send messages to processes, to check if they are
still alive, and to terminate them.

```ocaml
  let pid1 = spawn say_hello in
```

A common scenario is waiting for a number of pids to terminate. For this Riot
offers a `wait_pids` function that will return after all the pids are finished.

Be mindful that if the pids do not terminate, this function will get the caller
process stuck in that loop. We will see later in this tutorial more flexible
mechanisms for detecting when other processes terminate.

```ocaml
  wait_pids [pid1];
```


Finally, after our spawned process has terminated, we will simply log again.

```ocaml
  Logger.info (fun f -> f "%a has terminated" Pid.pp pid1)
```

Notice that when you run the program, the program itself _does not terminate_.
This is because we haven't ran `shutdown ()`. Calling it has the unfortunate
property that our Logger may not finish writing what it is meant to write.
