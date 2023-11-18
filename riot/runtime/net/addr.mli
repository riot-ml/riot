type 't raw_addr = string
type tcp_addr = string
type stream_addr = [ `Tcp of tcp_addr * int ]

val loopback : tcp_addr
val tcp : string -> 'a -> [> `Tcp of string * 'a ]
val to_unix : [< `Tcp of tcp_addr * int ] -> Unix.socket_type * Unix.sockaddr
val to_domain : [< `Tcp of 'a * 'b ] -> Unix.socket_domain
val of_unix : Unix.sockaddr -> [> `Tcp of tcp_addr * int ]
val pp : Format.formatter -> stream_addr -> unit
