type 't raw_addr = string
type tcp_addr = [ `v4 | `v6 ] raw_addr
type stream_addr = [ `Tcp of tcp_addr * int ]

module Ipaddr = struct
  let to_unix : tcp_addr -> Unix.inet_addr = Obj.magic
  let of_unix : Unix.inet_addr -> tcp_addr = Obj.magic
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
