open Runtime

type 'kind socket = Fd.t
type listen_socket = Fd.t
type stream_socket = Fd.t

type listen_opts = {
  reuse_addr : bool;
  reuse_port : bool;
  backlog : int;
  addr : string;
}

type timeout = Infinity | Bounded of float
type unix_error = [ `Unix_error of Unix.error ]

type nonrec ('ok, 'err) result = ('ok, 'err) result
  constraint 'err = [> unix_error ]

val default_listen_opts : listen_opts
val close : Runtime.Fd.t -> unit
val listen : ?opts:listen_opts -> port:int -> unit -> (Runtime.Fd.t, 'a) result

val connect :
  Net.Addr.stream_addr -> (Runtime.Fd.t, [> `Unix_error of Unix.error ]) result

val accept :
  ?timeout:timeout ->
  Runtime.Fd.t ->
  ( Net.Socket.stream_socket * Net.Addr.stream_addr,
    [> `Unix_error of Unix.error ] )
  result

val controlling_process : 'a -> new_owner:'b -> (unit, 'c) result

val receive :
  ?timeout:timeout ->
  len:int ->
  Runtime.Fd.t ->
  (Bigstringaf.t, [> `Unix_error of Unix.error ]) result

val send :
  Bigstringaf.t -> Runtime.Fd.t -> (int, [> `Unix_error of Unix.error ]) result
