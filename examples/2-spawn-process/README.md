# `2-spawn-process`

A _Process_ is a long-lived function, that has access to a mailbox to receive
messages.

Here's how we can create a process to print out the message from the last tutorial:

```ocaml
Riot.run @@ fun () ->
  let open Riot in
  let pid = spawn (fun () -> print_endline "Hello, Joe!") in
```

Riot has a `spawn` function that can be used to create a new process. Riot
processes are _cheap_, and Riot programs can have millions of processes. They
are not like Operating System processes (or threads), and are closer to
green-threads or fibers.

`spawn` takes a `unit -> unit` function as an input, and will give us back a
_pid_. A `Pid` is a Process Identifier. Pids are unique during the execution of a
program and can be used to send messages to processes, to check if they are
still alive, and to terminate them.

```ocaml
  let pid = spawn (fun () -> print_endline "Hello, Joe!") in
```

Inside of a process, we can get the pid of the process by calling `self ()`. A
Pid can also be pretty-printed with `Pid.pp` but it is not serializable.

```ocaml
  let pid = spawn (fun () -> Printf.printff "Hello, %a!" Pid.pp (self ())) in
```

A common scenario is waiting for a number of pids to terminate. For this Riot
offers a `wait_pids` function that will return after all the pids are finished.

Be mindful that if the pids do not terminate, this function will get the caller
process stuck in that wait loop. We will see later in this tutorial more
flexible mechanisms for detecting when other processes terminate.

```ocaml
  wait_pids [pid]
```

And as before, if we want the runtime to finish, we should call `shutdown ()`.

## Next Steps

* the [next step](../3-message-passing/) introduces you to communicating processes and Message passing
