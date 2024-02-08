# Contributing to Riot

Thanks for taking the time to contribute to Riot ✨ All contributions are
welcomed! This includes:

* PRs with bug fixes or features
* Examples
* Doc fixes
* Bug reports
* Links blogs featuring Riot
* Links to projects using Riot that can serve as large examples
* Links to libraries that can be used with Riot

### Installing from Sources

To install Riot from sources, make sure to include all its dependencies:

```sh
; opam pin config.0.0.1 git+https://github.com/leostera/config.ml
; opam pin libc.0.0.1 git+https://github.com/ocaml-sys/libc.ml
; opam pin rio.0.0.8 git+https://github.com/riot-ml/riot
; opam pin bytestring.0.0.8 git+https://github.com/riot-ml/riot
; opam pin gluon.0.0.8 git+https://github.com/riot-ml/riot
; opam pin riot.0.0.8 git+https://github.com/riot-ml/riot
```

You can run builds with:

```sh
; dune build
```

You can run all tests with

```sh
; dune test
```

### Adding tests

If you want to add a test, you can do so by creating a new OCaml file in the
`test` folder and updating `test/dune` to include a stanza for your test. The
boilerplate we use for a test is:

```ocaml
[@@@warning "-8"]
open Riot

let main () =
  let (Ok _) = Logger.start () in

  (* you can change this log level to Debug while you debug your tests *)
  Logger.set_log_level (Some Info);

  (* your test code *)
  let passed = true in

  match passed with
  | true ->
    Logger.info (fun f -> f "print that everything went well");
    shutdown ()
  | _ ->
    Logger.error (fun f -> f "print that something went wrong");
    sleep 0.1;
    Stdlib.exit 1

let () = Riot.run @@ main
```

Ideally tests will run `Riot.run` without configuring it too much, but it can
be helpful to reduce the number of schedulers you use while you're creating a
test to begin with, or if you're testing behavior of a single scheduler. To do
that you can set the `~workers` argument to `0`, so that no new schedulers are
created and you run only the main thread.

```ocaml
let () = Riot.run ~workers:0 @@ main
```

### Running tests

For the moment we rely on `dune test` to execute the small battery of tests.
Some of them take a long time (hello `spawn_many`), but they are helpful in
determining if we have bugs in a moderate number of processes (eg. 1M of them).

### Debugging Concurrency/Parallel Bugs

If you find or introduce a bug into Riot, a quick way to debug what is
happening is to enable TRACE logging in the runtime. Right now this is done
by manually setting the default log level to `(Some Trace)`.

```
Riot.Runtime.Log.set_log_level (Some Trace);
```

Once you do this, running your Riot program will emit _a lot of logs_. And it
will also run a lot slower. Trace logs (and any low-level logs) are implemented
using a lock over a stdout formatter, to ensure the outputs are consistent and
isn't being overwritten by other threads.

To make sense of these logs I recommend to:

1. Redirect this output to a file: `dune exec ./my_test.exe > logs`
2. Open the logs in your favorite editor to find the Pid that went bad (usually
   that got stuck in a loop)
3. Filter logs by pid: `cat logs | grep "0\.992\.0" | head -n 100`

This usually helps me find the sequence of actions for a Pid that tell me how
it got into its state.

You may find you need more information than is available. Feel free to add more
`Log.trace` calls all over Riot wherever you see fit, and submit them in a PR
if you think they'll help other people find bugs too.

`Log.*` functions are cheap if the logs are disabled, since the function you
pass to them only is evaluated when that log level is enabled.

## Performance

For doing performance work, it helps to use the `olly` tracer from the
`runtime_events_tools` package. 

```
; opam install runtime_events_tools -y
; olly trace riot.trace _build/default/examples/http_server/main.exe
```

`olly` will crate a trace file called `riot.trace` and you can open this file
in 2 steps:

1. run `./tools/trace_processor --httpd ./riot.trace` to preprocess the file
   (takes a bit)
2. go to `https://ui.perfetto.dev/` and click YES on the "Trace Processor
   Native Acceleration" dialogue

#### Basic Usage

Typically you'll want to find all the instances of a certain
operation that is slow. You can most likely see them straight
on in the viewer, like this:

1. click on a Process to see it expand
2. click on the trace name you're interested in (say `major_slice`)
3. in the details tab below click on the name 
4. click on "Slices with the same name"
5. click on Duration and sort by Highest First

That should show you the list of all the instances of the
trace you're looking for, sorted by the slowest ones.
