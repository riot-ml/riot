<h1 align="center"> riot </h1>

<p align="center">
An actor-model multi-core scheduler for OCaml 5.
</p>

Riot is an actor-model multi-core scheduler for OCaml 5. It brings Erlang-style
concurrency to the language, where lighweight process communicate via message
passing.

```ocaml
open Riot
type Message.t += Hello_world

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
        match receive () with
        | Hello_world ->
            Logger.info (fun f -> f "hello world from %a!" Pid.pp (self ())))
  in
  send pid Hello_world
```

It **features**:

* Dirt-cheap processes – spawn them by the millions
* Fast, type-safe message passing
* Selective receive expressions
* Process linking and monitoring
* Supervisors
* Async logging

### Non-goals

At the same time, there's a few things that Riot is not, and does not aim to be.

Primrarily, Riot is not a full port of the Erlang VM and it won't support several
of its use-cases, like:
* supporting Erlang or Elixir bytecode
* hot-code reloading in live applications
* function-call level tracing in live applications
* ad-hoc distribution

## Quick Start

```
git clone https://github.com/leostera/riot
cd riot
opam install .
```

After that, you can use any of the [examples][tutorial] as a base for your app, and run them:

```
dune exec ./my_app.exe
```

## Acknowledgments

Riot is the continuation of the work I started with [Caramel](https://github.com/leostera/caramel), an Erlang-backend for the OCaml compiler.

It was heavily inspired by [eio][eio] by the OCaml Multicore team and
[miou][miou] by [Calascibetta Romain](https://twitter.com/Dinoosaure) and the
[Robur team](https://robur.coop/), as I learned more about Algebraic Effects. In particular the `Proc_state` is based on the `State` module in Miou.

And a thousand thanks to [Calascibetta Romain](https://twitter.com/Dinoosaure)
and [Antonio Monteiro](https://twitter.com/_anmonteiro) for the discussions and
feedback.

[eio]: https://github.com/ocaml-multicore/eio
[miou]: https://github.com/robur-coop/miou
