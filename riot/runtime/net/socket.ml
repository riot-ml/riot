open Util

type 'kind socket = Fd.t
type listen_socket = [ `listen ] socket
type stream_socket = [ `stream ] socket
