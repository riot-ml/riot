# `http_server`

This is a more complex example where we build an HTTP server from scratch,
starting with a wrapper around UNIX sockets that plays more nicely with Riot,
and then building a supervision hierarchy for accepting connections and passing
them on through a protocol handler.

Our simple server is then started from the `main.ml` file just like any other process-based server is, calling `start_link`.

```ocaml
let (Ok _server) = Http_server.start_link ~port @@ fun reqd -> ... in
```

The `Http_server` module configured the more interesteing `Server` module to use a `Tcp_connector` configured to handle requests using `http/af`. That bit of configuration is crucial, otherwise our server wouldn't know what protocol the client is trying to talk to, but it is irrelevant to how the application is designed.

In fact, the entire Server itself would work for any protocol as long as we can
implements a `Socket.Flow.t` value to read/write it.

## The Server Supervision Tree

For this web server, I modeled the supervision tree after the [ranch][ranch]
Erlang library. It looks roughly like this

```mermaid
graph TD
    Main

    Server_sup(Server.Supervisor)
    Acceptor_sup(Acceptor.Supervisor)
    Acceptor_wrk[Acceptor.Worker]
    Connector_wrk[Connector]
    Writer([Writer])
    Reader([Reader])

    Main --> |start_link| Server_sup

    Server_sup --> Acceptor_sup
    Acceptor_sup --> |bind socket|Acceptor_sup

    Acceptor_sup --> Acceptor_wrk
    Acceptor_sup --> Acceptor_wrk1[...N acceptors]

    Acceptor_wrk --> |accept loop| Acceptor_wrk
    

    Acceptor_wrk --> |on new client| Connector_wrk
    Connector_wrk --> |spawn| Writer & Reader

    Reader --> |read| Socket
    Writer --> |write| Socket
```

It all starts with the _Server supervisor_, which spins up 1 _Acceptor supervisor_.
This pattern of supervisors supervising supervisors is very common for structuring applications in Erlang.

The _Acceptor supervisor_ binds to a fresh TCP socket and creates N _Acceptor
workers_. Every worker enters an _accept loop_ where they await for new
connections on the shared TCP socket.

Once a client connects, the acceptor will spawn a new _Connector process_, link
it, initiates the Connector's handshake process, add its pid to the list of
connections, and continue awaiting for the next client by recursing over
`accept_loop`.

This loop lets us immediately offload the work of handling a connection to a
new process (potentially in a separate scheduler thread), and instantly resume
listening for new connections.

The _Connector process_ receives the connection (a file descriptor) for this
specific server/client interaction, and will await the beginning of the
handshake process. This is used to indicate that the Acceptor is finished with
the setup, and the Connector can start its work. This coordination happens via
the `Connector.Handshake` message.

The Connector then spawns and links a writer and a reader process, which loop
over the socket and use a `Socket.Flow.t` to configure how to consume it. The
API of `Socket.Flow.t` is modeled very close to how `http/af` works, since that
was the only requirement this example had. 


[ranch]: https://github.com/ninenines/ranch
