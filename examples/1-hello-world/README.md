# `1-hello-world`

A project so basic that fits on a single line!

```ocaml
Riot.run @@ fun () -> print_endline "Hello, Joe!"
```

Every Riot program begins with a call to `Riot.run`. `Riot.run` takes a single
function as input, and _does not terminate_. This is because Riot programs are
expected to _run forever_.

If you want to terminate the Riot runtime, you can call `Riot.shutdown ()` from
anywhere in the program. Keep in mind that this will not await for all
processes to terminate. We will cover graceful-shutdowns of applications later
in this tutorial.

```ocaml
Riot.run @@ fun () ->
    print_endline "Hello, Joe!";
    Riot.shutdown ()
```

The smallest Riot program, that starts and ends immediately, is then:

```ocaml
Riot.(run shutdown)
```

## Next Steps

* the [next step](../2-spawn-process/) introduces you to Processes
