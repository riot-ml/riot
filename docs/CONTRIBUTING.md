# Contributing to Riot

Thanks for taking the time to contribute to Riot. All contributions are welcomed! This includes:

* PRs with bug fixes or features
* Examples
* Doc fixes
* Bug reports
* Links blogs featuring Riot
* Links to projects using Riot that can serve as large examples
* Links to libraries that can be used with Riot

### Adding tests

If you want to add a test, you can do so by creating a new OCaml file in the `test` folder. The boilerplate we use for a test is:

```
[@@@warning "-8"]
open Riot

let main () =
  let Ok () = Logger.start () in

  (* your test code *)

  match passed with
  | true ->
    Logger.info (fun f -> f "print that everything went well");
    shutdown ()
  | _ ->
    Logger.error (fun f -> f "print that something went wrong);
    Stdlib.exit 1

let () = Riot.run @@ main
```

Ideally tests will run `Riot.run` without configuring it too much, but it can
be helpful to reduce the number of schedulers you use while you're creating a
test to begin with, or if you're testing behavior of a single scheduler. To do
that you can set the `~workers` argument to `0`, so that no new schedulers are
created and you run only the main thread.

```
let () = Riot.run ~workers:0 @@ main
```

### Running tests

For the moment we rely on `dune test` to execute the small battery of tests.
Some of them take a long time (hello `spawn_many`), but they are helpful in
determining if we have bugs in a moderate number of processes (eg. 1M of them).

### Debugging Concurrency/Parallel Bugs

If you find or introduce a bug into Riot, a quick way to debug what is
happening is to enable TRACE logging in `lib/logs.ml`. Right now this is done
by manually setting the default log level to `(Some Trace)`.

Once you do this, running your Riot program will emit _a lot of logs_. And it
will also run a hell of a lot slower. Trace logs (and any low-level logs) are
implemented using a lock over a stdout formatter, to ensure the outputs are
consistent and isn't being overwritten by other threads.

To make sense of these logs I recommend to:

1. Redirect this output to a file: `dune exec ./my_test.exe > logs`
2. Open the logs in your favorite editor to find the Pid that went bad (usually
   that got stuck in a loop)
3. Filter logs by pid: `cat logs | grep "0\.992\.0" | head -n 100`

This usually helps me find the sequence of actions for a Pid that tell me how
it got into its state.

You may find you need more information than is available. Feel free to add more
`Logs.trace` calls all over Riot wherever you see fit, and submit them in a PR
if you think they'll help other people find bugs too.

`Logs.*` functions are cheap if the logs are disabled, since the function you
pass to them only is evaluated when that log level is enabled.
