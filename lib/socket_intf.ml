module type Intf = sig
  type 'kind socket
  type listen_socket = [ `listen ] socket
  type stream_socket = [ `stream ] socket
  type listen_opts = { backlog : int }
  type addr
  type timeout = Infinity | Bounded of float
  type unix_error = [ `Unix_error of Unix.error ]
  type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

  val listen :
    ?opts:listen_opts -> port:int -> (listen_socket, [> `System_limit ]) result

  val accept :
    ?timeout:timeout ->
    listen_socket ->
    (stream_socket, [> `Closed | `Timeout | `System_limit ]) result

  val close : 'kind socket -> (unit, [> ]) result

  val controlling_process :
    'kind socket -> new_owner:Pid.t -> (unit, [> `Closed | `Not_owner ]) result

  val receive :
    ?timeout:timeout ->
    len:int ->
    stream_socket ->
    (Bigstringaf.t, [> `Closed | `Timeout ]) result

  val send : Bigstringaf.t -> stream_socket -> (unit, [> `Closed ]) result
end
