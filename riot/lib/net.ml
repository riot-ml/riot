module Net = Runtime.Net
module Low_level = Runtime.Io
open Global

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "net" ]
end)

let ( let* ) = Result.bind

module Addr = struct
  include Net.Addr

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
    match Low_level.getaddrinfo host service with
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
    Low_level.close pool.io_scheduler.io_tbl socket

  let listen ?(opts = default_listen_opts) ~port () =
    let pool = Scheduler.Pool.get_pool () in
    let { reuse_addr; reuse_port; backlog; addr } = opts in
    let addr = Addr.tcp addr port in
    Logger.trace (fun f -> f "Listening on 0.0.0.0:%d" port);
    Low_level.listen pool.io_scheduler.io_tbl ~reuse_port ~reuse_addr ~backlog
      addr

  let rec connect addr =
    let pool = Scheduler.Pool.get_pool () in
    Logger.trace (fun f -> f "Connecting to %a" Addr.pp addr);
    match Low_level.connect pool.io_scheduler.io_tbl addr with
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

  let rec accept ?(timeout = `infinity) (socket : listen_socket) =
    let pool = Scheduler.Pool.get_pool () in
    Log.trace (fun f -> f "Socket is Accepting client at fd=%a" Fd.pp socket);
    match Low_level.accept pool.io_scheduler.io_tbl socket with
    | exception Fd.(Already_closed _) -> Error `Closed
    | `Abort reason -> Error (`Unix_error reason)
    | `Retry -> syscall "accept" `r socket @@ accept ~timeout
    | `Connected (conn, addr) -> Ok (conn, addr)

  let controlling_process _socket ~new_owner:_ = Ok ()

  let receive ?timeout ~buf socket =
    let rec receive_loop ~buf socket =
      Logger.trace (fun f ->
          f "receiving up to %d octets" (Io.Buffer.length buf));
      match Low_level.readv socket [| Io.Buffer.as_cstruct buf |] with
      | exception Fd.(Already_closed _) -> Error `Closed
      | `Abort reason -> Error (`Unix_error reason)
      | `Retry ->
          Logger.trace (fun f -> f "waiting on socket to receive");
          syscall "receive" `r socket @@ receive_loop ~buf
      | `Read 0 -> Error `Closed
      | `Read len ->
          Logger.trace (fun f -> f "received: %d octets" len);
          Io.Buffer.set_filled buf ~filled:len;
          Ok len
    in
    match timeout with
    | None -> receive_loop ~buf socket
    | Some timeout ->
        Logger.trace (fun f -> f "receive with timeout %Ld" timeout);
        let task = Task.async (fun () -> receive_loop ~buf socket) in
        let* result = Task.await ~timeout task in
        result

  let send ?timeout ~data socket =
    let rec send_loop ~data socket =
      Logger.trace (fun f -> f "sending: %d octets" (Io.Buffer.length data));
      match Low_level.writev socket [| Io.Buffer.as_cstruct data |] with
      | exception Fd.(Already_closed _) -> Error `Closed
      | `Abort reason -> Error (`Unix_error reason)
      | `Retry ->
          Logger.trace (fun f -> f "retrying");
          syscall "send" `w socket @@ send_loop ~data
      | `Wrote bytes ->
          Logger.trace (fun f -> f "sent: %d" (Io.Buffer.length data));
          Ok bytes
    in
    match timeout with
    | None -> send_loop ~data socket
    | Some timeout ->
        Logger.trace (fun f -> f "send with timeout %Ld" timeout);
        let task = Task.async (fun () -> send_loop ~data socket) in
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
    let module Read = Io.Reader.Make (struct
      type t = stream_socket

      let read t ~buf = receive ?timeout ~buf t
    end) in
    Io.Reader.of_read_src (module Read) t

  let to_writer ?timeout t =
    let module Write = Io.Writer.Make (struct
      type t = stream_socket

      let write t ~data = send ?timeout ~data t
      let flush _t = Ok ()
    end) in
    Io.Writer.of_write_src (module Write) t
end
