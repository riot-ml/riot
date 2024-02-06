open Gluon
open Global

open Logger.Make (struct
  let namespace = [ "riot"; "net" ]
end)

module Socket = Gluon.Net.Socket
module Addr = Gluon.Net.Addr

module Tcp_listener = struct
  include Gluon.Net.Tcp_listener

  type listen_opts = {
    reuse_addr : bool;
    reuse_port : bool;
    backlog : int;
    addr : Addr.tcp_addr;
  }

  let default_listen_opts =
    {
      reuse_addr = true;
      reuse_port = true;
      backlog = 128;
      addr = Addr.loopback;
    }

  let bind ?(opts = default_listen_opts) ~port () =
    let { reuse_addr; reuse_port; backlog; addr } = opts in
    let addr = Addr.tcp addr port in
    trace (fun f -> f "Listening on 0.0.0.0:%d" port);
    bind ~reuse_port ~reuse_addr ~backlog addr

  let accept ?timeout t =
    let this = self () in
    let rec accept_loop t =
      trace (fun f -> f "Socket is Accepting client at fd=%a" Fd.pp t);
      match accept t with
      | Ok (conn, addr) ->
          trace (fun f ->
              f "Accepted client %a / %a" Addr.pp addr Socket.pp conn);
          Ok (conn, addr)
      | Error `Would_block ->
          trace (fun f ->
              f "Socket not ready, %a is retrying at fd=%a" Pid.pp this Fd.pp t);
          syscall "accept" Interest.(add readable writable) (to_source t)
          @@ fun () -> accept_loop t
      | Error err -> Error err
    in

    match timeout with
    | None -> accept_loop t
    | Some timeout ->
        trace (fun f -> f "accept with timeout %Ld" timeout);
        let task = Task.async (fun () -> accept_loop t) in
        let* result = Task.await ~timeout task in
        result

  let close t =
    let this = self () in
    trace (fun f -> f "Process %a: Closing socket fd=%a" Pid.pp this Fd.pp t);
    close t
end

module Tcp_stream = struct
  include Gluon.Net.Tcp_stream

  let close t =
    let this = self () in
    trace (fun f -> f "Process %a: Closing socket fd=%a" Pid.pp this Fd.pp t);
    close t

  let with_timeout ?timeout fn =
    match timeout with
    | None -> fn ()
    | Some timeout ->
        let task = Task.async fn in
        let* result = Task.await ~timeout task in
        result

  let connect ?timeout addr =
    let rec connect_loop addr =
      trace (fun f -> f "Attempting to connect to %a" Addr.pp addr);
      match connect addr with
      | Ok (`Connected t) ->
          trace (fun f -> f "Connected to %a" Addr.pp addr);
          Ok t
      | Ok (`In_progress t) ->
          trace (fun f -> f "In_progress %a" Addr.pp addr);
          syscall "connect" Interest.(writable) (to_source t) @@ fun () -> Ok t
      | Error `Would_block ->
          yield ();
          connect_loop addr
      | Error err -> Error err
    in
    with_timeout ?timeout @@ fun () -> connect_loop addr

  let rec receive ?timeout ~bufs t =
    trace (fun f ->
        f "receiving up to %d octets from %a" (Io.Iovec.length bufs) Socket.pp t);
    match read_vectored t bufs with
    | Ok len ->
        trace (fun f -> f "received: %d octets from %a" len Socket.pp t);
        Ok len
    | Error `Would_block ->
        trace (fun f -> f "waiting on %a to receive" Socket.pp t);
        syscall ?timeout "receive" Interest.readable (to_source t) @@ fun () ->
        receive ?timeout ~bufs t
    | Error err -> Error err

  let rec send ?timeout ~bufs t =
    trace (fun f -> f "sending: %d octets" (Io.Iovec.length bufs));
    match write_vectored t bufs with
    | Ok bytes ->
        trace (fun f -> f "sent: %d" (Io.Iovec.length bufs));
        Ok bytes
    | Error `Would_block ->
        trace (fun f -> f "retrying");
        syscall ?timeout "send" Interest.writable (to_source t) @@ fun () ->
        send ?timeout ~bufs t
    | Error err -> Error err

  let pp_err fmt = function
    | `Timeout -> Format.fprintf fmt "Timeout"
    | `Process_down -> Format.fprintf fmt "Process_down"
    | `System_limit -> Format.fprintf fmt "System_limit"
    | `Closed -> Format.fprintf fmt "Closed"
    | `Unix_error err ->
        Format.fprintf fmt "Unix_error(%s)" (Unix.error_message err)

  let to_reader ?timeout:global_timeout t =
    let module Read = struct
      type nonrec t = t

      let read t ?timeout buf =
        let timeout =
          match timeout with None -> global_timeout | Some _ -> timeout
        in
        receive ?timeout ~bufs:(Io.Iovec.of_bytes buf) t

      let read_vectored t bufs = receive ?timeout:global_timeout ~bufs t
    end in
    Io.Reader.of_read_src (module Read) t

  let to_writer ?timeout t =
    let module Write = struct
      type nonrec t = t

      let write_owned_vectored t ~bufs = send ?timeout ~bufs t

      let write t ~buf =
        let bufs = Io.Iovec.from_string buf in
        write_owned_vectored t ~bufs

      let flush _t = Ok ()
    end in
    Io.Writer.of_write_src (module Write) t
end
