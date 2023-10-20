type 't raw_addr = string
type tcp_addr = [ `v4 | `v6 ] raw_addr
type stream_addr = [ `Tcp of tcp_addr * int ]

let loopback : tcp_addr = "0.0.0.0"
let tcp host port = `Tcp (host, port)

let to_unix addr =
  match addr with
  | `Tcp (host, port) ->
      let addr = Unix.inet_addr_of_string host in
      (Unix.SOCK_STREAM, Unix.ADDR_INET (addr, port))

let to_domain addr = match addr with `Tcp (_host, _) -> Unix.PF_INET

let of_unix sockaddr =
  match sockaddr with
  | Unix.ADDR_INET (host, port) -> `Tcp (Obj.magic host, port)
  | Unix.ADDR_UNIX _ -> failwith "unsupported unix addresses"

let pp ppf (addr : stream_addr) =
  match addr with `Tcp (host, port) -> Format.fprintf ppf "%s:%d" host port
