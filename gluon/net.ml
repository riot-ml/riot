open Gluon_common
open Gluon_sys

module Addr = struct
  type 't raw_addr = string
  type tcp_addr = [ `v4 | `v6 ] raw_addr
  type stream_addr = [ `Tcp of tcp_addr * int ]

  module Ipaddr = struct
    let to_unix : tcp_addr -> Unix.inet_addr = Unix.inet_addr_of_string
    let of_unix : Unix.inet_addr -> tcp_addr = Unix.string_of_inet_addr
  end

  let loopback : tcp_addr = "0.0.0.0"

  let tcp host port =
    assert (String.length host > 0);
    `Tcp (host, port)

  let to_unix addr =
    match addr with
    | `Tcp (host, port) ->
        (Unix.SOCK_STREAM, Unix.ADDR_INET (Ipaddr.to_unix host, port))

  let to_domain addr = match addr with `Tcp (_host, _) -> Unix.PF_INET

  let of_unix sockaddr =
    match sockaddr with
    | Unix.ADDR_INET (host, port) -> tcp (Ipaddr.of_unix host) port
    | Unix.ADDR_UNIX addr -> failwith ("unsupported unix addresses: " ^ addr)

  let pp ppf (addr : stream_addr) =
    match addr with `Tcp (host, port) -> Format.fprintf ppf "%s:%d" host port

  let to_string t = t

  let of_addr_info Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; _ } =
    match (ai_family, ai_socktype, ai_addr) with
    | ( (Unix.PF_INET | Unix.PF_INET6),
        (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
        Unix.ADDR_INET (addr, port) ) -> (
        match ai_protocol with
        | 6 -> Some (tcp (Unix.string_of_inet_addr addr) port)
        | _ -> None)
    | _ ->
        Printf.printf "skipping\n%!";
        None

  let get_info host service =
    syscall @@ fun () ->
    let info = Unix.getaddrinfo host service [] in
    Ok (List.filter_map of_addr_info info)

  let of_uri uri =
    let port =
      match Uri.port uri with
      | Some port -> Int.to_string port
      | _ -> Uri.scheme uri |> Option.value ~default:"http"
    in
    let host = Uri.host_with_default ~default:"0.0.0.0" uri in
    match get_info host port with
    | Ok (ip :: _) -> Ok ip
    | Ok [] -> Error `No_info
    | Error err -> Error err

  let parse str = Uri.of_string str |> of_uri
  let get_info (`Tcp (host, port)) = get_info host (Int.to_string port)
  let ip (`Tcp (ip, _)) = ip
  let port (`Tcp (_, port)) = port
end

module Socket = struct
  type 'kind socket = Fd.t
  type listen_socket = [ `listen ] socket
  type stream_socket = [ `stream ] socket

  let pp fmt t = Fd.pp fmt t
  let close t = Unix.close t

  let make sock_domain sock_type =
    let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
    Unix.set_nonblock fd;
    Fd.make fd
end

module Tcp_listener = struct
  type t = Socket.listen_socket

  let pp = Socket.pp
  let close = Socket.close

  let bind ?(reuse_addr = true) ?(reuse_port = true) ?(backlog = 128) addr =
    syscall @@ fun () ->
    let sock_domain = Addr.to_domain addr in
    let sock_type, sock_addr = Addr.to_unix addr in
    let fd = Socket.make sock_domain sock_type in
    Unix.setsockopt fd Unix.SO_REUSEADDR reuse_addr;
    Unix.setsockopt fd Unix.SO_REUSEPORT reuse_port;
    Unix.bind fd sock_addr;
    Unix.listen fd backlog;
    Ok fd

  let accept fd =
    syscall @@ fun () ->
    let raw_fd, client_addr = Unix.accept ~cloexec:true fd in
    Unix.set_nonblock raw_fd;
    let addr = Addr.of_unix client_addr in
    let fd = Fd.make raw_fd in
    Ok (fd, addr)

  let to_source t =
    let module Src = struct
      type nonrec t = t

      let register t selector token interest =
        Sys.Selector.register selector ~fd:t ~token ~interest

      let reregister t selector token interest =
        Sys.Selector.reregister selector ~fd:t ~token ~interest

      let deregister t selector = Sys.Selector.deregister selector ~fd:t
    end in
    Source.make (module Src) t
end

module Tcp_stream = struct
  open Io

  type t = Socket.stream_socket

  let pp = Socket.pp
  let close = Socket.close

  let connect addr =
    let sock_domain = Addr.to_domain addr in
    let sock_type, sock_addr = Addr.to_unix addr in
    let fd = Socket.make sock_domain sock_type in
    syscall @@ fun () ->
    try
      Unix.connect fd sock_addr;
      Ok (`Connected fd)
    with Unix.(Unix_error (EINPROGRESS, _, _)) -> Ok (`In_progress fd)

  let read fd ?(pos = 0) ?len buf =
    let len = Option.value len ~default:(Bytes.length buf - 1) in
    syscall @@ fun () -> Ok (UnixLabels.read fd ~buf ~pos ~len)

  let write fd ?(pos = 0) ?len buf =
    let len = Option.value len ~default:(Bytes.length buf - 1) in
    syscall @@ fun () -> Ok (UnixLabels.write fd ~buf ~pos ~len)

  external gluon_readv : Unix.file_descr -> Iovec.t -> int = "gluon_unix_readv"

  let read_vectored fd iov = syscall @@ fun () -> Ok (gluon_readv fd iov)

  external gluon_writev : Unix.file_descr -> Iovec.t -> int
    = "gluon_unix_writev"

  let write_vectored fd iov = syscall @@ fun () -> Ok (gluon_writev fd iov)

  external gluon_sendfile :
    Unix.file_descr -> Unix.file_descr -> int -> int -> int
    = "gluon_unix_sendfile"

  let sendfile fd ~file ~off ~len =
    syscall @@ fun () -> Ok (gluon_sendfile file fd off len)

  let to_source t =
    let module Src = struct
      type nonrec t = t

      let register t selector token interest =
        Sys.Selector.register selector ~fd:t ~token ~interest

      let reregister t selector token interest =
        Sys.Selector.reregister selector ~fd:t ~token ~interest

      let deregister t selector = Sys.Selector.deregister selector ~fd:t
    end in
    Source.make (module Src) t
end
