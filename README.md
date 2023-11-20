<h1 align="center"> riot </h1>

<p align="center">
An actor-model multi-core scheduler for OCaml 5.
</p>

Riot is an [actor-model][actors] multi-core scheduler for OCaml 5. It brings
[Erlang][erlang]-style concurrency to the language, where lightweight processes communicate via message-passing.

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

At its core Riot aims to offer:

* **Automatic multi-core scheduling** – when you spawn a new Riot process, it
  will automatically get allocated on a random scheduler.

* **Lightweight processes** – spawn 10 or 10,000 processes as you see fit.

* **Fast, type-safe message passing**

* **Selective receive expressions** – when receiving messages, you can skim
  through a process mailbox to consume them in arbitrary order.

* **Process links and monitors** to keep track of the lifecycle of processes

Riot also includes:

* **Supervisors** to build process hierarchies

* **Logging** and **Telemetry** designed to be multicore friendly

* an **Application** interface to orchestrate startup/shutdown of systems

* **Generic Servers** for designing encapsulated services like with Elixir's [GenServer][genserver]

### Non-goals

At the same time, there's a few things that Riot is not, and does not aim to be.

Primarily, Riot is not a full port of the Erlang VM and it won't support
several of its use-cases, like:
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

After that, you can use any of the [examples](./examples) as a base for your app, and run them:

```
dune exec ./my_app.exe
```

## Acknowledgments

Riot is the continuation of the work I started with
[Caramel](https://github.com/leostera/caramel), an Erlang-backend for the OCaml
compiler.

It was heavily inspired by [eio][eio] by the OCaml Multicore team and
[miou][miou] by [Calascibetta Romain](https://twitter.com/Dinoosaure) and the
[Robur team](https://robur.coop/), as I learned more about Algebraic Effects.
In particular the `Proc_state` is based on the `State` module in Miou.

And a thousand thanks to [Calascibetta Romain](https://twitter.com/Dinoosaure)
and [Antonio Monteiro](https://twitter.com/_anmonteiro) for the discussions and
feedback.

[actors]: https://en.wikipedia.org/wiki/Actor_model
[erlang]: https://erlang.org
[eio]: https://github.com/ocaml-multicore/eio
[miou]: https://github.com/robur-coop/miou
[genserver]: https://hexdocs.pm/elixir/1.12/GenServer.html
