[@@@config any (target_os = "linux", target_os = "android")]

open Gluon_common

type epoll
type event = { u64 : int; events : int }

module FFI = struct
  external epoll_create1 : flags:int -> epoll = "gluon_unix_epoll_create1"

  external epoll_wait : timeout:int64 -> max_events:int -> epoll -> event array
    = "gluon_unix_epoll_wait"

  external epoll_ctl : epoll -> flags:int -> fd:Fd.t -> event -> unit
    = "gluon_unix_epoll_ctl"

  external epoll_ctl_null : epoll -> flags:int -> fd:Fd.t -> unit
    = "gluon_unix_epoll_ctl_null"

  let epoll_wait ~timeout ~max_events epoll =
    syscall @@ fun () -> Ok (epoll_wait ~timeout ~max_events epoll)

  let epoll_ctl_null epoll ~flags ~fd =
    syscall @@ fun () -> Ok (epoll_ctl_null epoll ~flags ~fd)

  let epoll_ctl epoll ~flags ~fd event =
    syscall @@ fun () ->
      try Ok (epoll_ctl epoll ~flags ~fd event)
      with Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
end

module Event = struct
  type t = event

  let token t = Token.make t.u64

  let is_readable t =
    t.events land Libc.epollin != 0 || t.events land Libc.epollpri != 0

  let is_writable t = t.events land Libc.epollout != 0
  let is_error t = t.events land Libc.epollerr != 0

  let is_read_closed t =
    t.events land Libc.epollhup != 0
    || (t.events land Libc.epollin != 0 && t.events land Libc.epollrdhup != 0)

  let is_write_closed t =
    t.events land Libc.epollhup != 0
    || (t.events land Libc.epollout != 0 && t.events land Libc.epollerr != 0)
    || t.events == Libc.epollerr

  let is_priority _t = false
end

module Selector = struct
  let name = "epoll"

  type t = { ep : epoll }

  let make () = Ok { ep = FFI.epoll_create1 ~flags:Libc.epoll_cloexec }

  let select ?(timeout = 1L) ?(max_events = 1_000) t =
    let* events = FFI.epoll_wait ~timeout ~max_events t.ep in
    let events = Array.to_list events in
    let events = List.map (Gluon_events.Event.make (module Event)) events in
    Ok events

  let interests_to_epoll interest =
    let open Libc in
    let kind = epollet in
    let kind =
      if Interest.is_readable interest then kind lor epollin lor epollrdhup
      else kind
    in
    let kind =
      if Interest.is_writable interest then kind lor epollout else kind
    in
    kind

  let register t ~fd ~token ~interest =
    let u64 = Token.unsafe_to_int token in
    let events = interests_to_epoll interest in
    let event = { u64; events } in
    FFI.epoll_ctl t.ep ~flags:Libc.epoll_ctl_add ~fd event

  let reregister t ~fd ~token ~interest =
    let u64 = Token.unsafe_to_int token in
    let events = interests_to_epoll interest in
    let event = { u64; events } in
    FFI.epoll_ctl t.ep ~flags:Libc.epoll_ctl_mod ~fd event

  let deregister t ~fd = FFI.epoll_ctl_null t.ep ~flags:Libc.epoll_ctl_del ~fd
end
