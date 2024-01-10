module Net = Runtime.Net
open Global

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "net" ]
end)

let ( let* ) = Result.bind

module Addr = struct
  include Net.Addr

  let to_string t = t

  let of_addr_info
      Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; ai_canonname } =
    match (ai_family, ai_socktype, ai_addr) with
    | ( (Unix.PF_INET | Unix.PF_INET6),
        (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
        Unix.ADDR_INET (addr, port) ) -> (
        Logger.trace (fun f ->
            f "of_addr_info %s or %s" ai_canonname (Obj.magic addr));
        match ai_protocol with
        | 6 -> Some (tcp (Unix.string_of_inet_addr addr) port)
        | _ -> None)
    | _ -> None

  let rec get_info host service =
    match Gluon.getaddrinfo host service with
    | `Ok info -> List.filter_map of_addr_info info
    | `Retry ->
        yield ();
        get_info host service
    | `Abort _err -> failwith "getaddrinfo failed"

  let of_uri uri =
    let port =
      match Uri.port uri with
      | Some port -> Int.to_string port
      | _ -> Uri.scheme uri |> Option.value ~default:"http"
    in
    let host = Uri.host_with_default ~default:"0.0.0.0" uri in
    Logger.trace (fun f -> f "host: %s port: %s" host port);
    match get_info host port with ip :: _ -> Some ip | [] -> None

  let get_info (`Tcp (host, port)) = get_info host (Int.to_string port)
  let ip (`Tcp (ip, _)) = ip
  let port (`Tcp (_, port)) = port
end

module Socket = struct
  include Net.Socket

  type listen_opts = {
    reuse_addr : bool;
    reuse_port : bool;
    backlog : int;
    addr : Addr.tcp_addr;
  }

  type unix_error = [ `Unix_error of Unix.error ]
  type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

  let default_listen_opts =
    {
      reuse_addr = true;
      reuse_port = true;
      backlog = 128;
      addr = Addr.loopback;
    }

  let close socket =
    let pool = Scheduler.Pool.get_pool () in
    let this = self () in
    Logger.trace (fun f ->
        f "Process %a: Closing socket fd=%a" Pid.pp this Fd.pp socket);
    Gluon.close pool.io_scheduler.io_tbl socket

  let listen ?(opts = default_listen_opts) ~port () =
    let pool = Scheduler.Pool.get_pool () in
    let { reuse_addr; reuse_port; backlog; addr } = opts in
    let addr = Addr.tcp addr port in
    Logger.trace (fun f -> f "Listening on 0.0.0.0:%d" port);
    Gluon.listen pool.io_scheduler.io_tbl ~reuse_port ~reuse_addr ~backlog addr

  let rec connect addr =
    let pool = Scheduler.Pool.get_pool () in
    Logger.trace (fun f -> f "Connecting to %a" Addr.pp addr);
    match Gluon.connect pool.io_scheduler.io_tbl addr with
    | `Connected fd -> connected addr fd
    | `In_progress fd -> in_progress addr fd
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry ->
        yield ();
        connect addr

  and in_progress addr fd = syscall "connect" `w fd @@ connected addr

  and connected addr fd =
    Logger.trace (fun f -> f "Connecting to %a via %a" Addr.pp addr pp fd);
    Ok fd

  let accept ?timeout (socket : listen_socket) =
    let this = self () in
    let rec accept_loop socket =
      Logger.trace (fun f ->
          f "Socket is Accepting client at fd=%a" Fd.pp socket);
      match Gluon.accept socket with
      | exception Fd.(Already_closed _) -> Error `Closed
      | `Abort reason -> Error (`Unix_error reason)
      | `Retry ->
          Log.debug (fun f ->
              f "Socket not ready, %a is retrying at fd=%a" Pid.pp this Fd.pp
                socket);
          syscall "accept" `r socket @@ accept_loop
      | `Connected (conn, addr) -> Ok (conn, addr)
    in

    match timeout with
    | None -> accept_loop socket
    | Some timeout ->
        Logger.trace (fun f -> f "accept with timeout %Ld" timeout);
        let task = Task.async (fun () -> accept_loop socket) in
        let* result = Task.await ~timeout task in
        result

  let controlling_process _socket ~new_owner:_ = Ok ()

  let receive ?timeout ~bufs socket =
    let rec receive_loop ~bufs socket =
      Logger.trace (fun f ->
          f "receiving up to %d octets" (Io.Iovec.length bufs));
      match Gluon.readv socket bufs with
      | exception Fd.(Already_closed _) -> Error `Closed
      | `Abort reason -> Error (`Unix_error reason)
      | `Retry ->
          Logger.trace (fun f -> f "waiting on socket to receive");
          syscall "receive" `r socket @@ receive_loop ~bufs
      | `Read 0 -> Error `Closed
      | `Read len ->
          Logger.trace (fun f -> f "received: %d octets" len);
          Ok len
    in
    match timeout with
    | None -> receive_loop ~bufs socket
    | Some timeout ->
        Logger.trace (fun f -> f "receive with timeout %Ld" timeout);
        let task = Task.async (fun () -> receive_loop ~bufs socket) in
        let* result = Task.await ~timeout task in
        result

  let send ?timeout ~bufs (socket : stream_socket) =
    let rec send_loop ~bufs socket =
      Logger.trace (fun f -> f "sending: %d octets" (Io.Iovec.length bufs));
      match Gluon.writev socket bufs with
      | exception Fd.(Already_closed _) -> Error `Closed
      | `Abort reason -> Error (`Unix_error reason)
      | `Retry ->
          Logger.trace (fun f -> f "retrying");
          syscall "send" `w socket @@ send_loop ~bufs
      | `Wrote bytes ->
          Logger.trace (fun f -> f "sent: %d" (Io.Iovec.length bufs));
          Ok bytes
    in
    match timeout with
    | None -> send_loop ~bufs socket
    | Some timeout ->
        Logger.trace (fun f -> f "send with timeout %Ld" timeout);
        let task = Task.async (fun () -> send_loop ~bufs socket) in
        let* result = Task.await ~timeout task in
        result

  let pp_err fmt = function
    | `Timeout -> Format.fprintf fmt "Timeout"
    | `Process_down -> Format.fprintf fmt "Process_down"
    | `System_limit -> Format.fprintf fmt "System_limit"
    | `Closed -> Format.fprintf fmt "Closed"
    | `Unix_error err ->
        Format.fprintf fmt "Unix_error(%s)" (Unix.error_message err)

  let to_reader ?timeout t =
    let module Read = struct
      type t = stream_socket

      let read t ~buf = receive ?timeout ~bufs:(Io.Iovec.of_bytes buf) t
      let read_vectored t ~bufs = receive ?timeout ~bufs t
    end in
    Io.Reader.of_read_src (module Read) t

  let to_writer ?timeout t =
    let module Write = struct
      type t = stream_socket

      let write_owned_vectored t ~bufs = send ?timeout ~bufs t

      let write t ~buf =
        let bufs = Io.Iovec.of_bytes buf in
        write_owned_vectored t ~bufs

      let flush _t = Ok ()
    end in
    Io.Writer.of_write_src (module Write) t
end
