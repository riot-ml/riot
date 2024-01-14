open Gluon_common

type epoll = Fd.t
type event = { u64 : int64; events : int }

module FFI = struct
  external epoll_create1 : flags:int -> epoll = "gluon_unix_epoll_create1"

  external epoll_wait : timeout:int64 -> max_events:int -> epoll -> event array
    = "gluon_unix_epoll_wait"

  external epoll_ctl : epoll -> flags:int -> fd:Fd.t -> event -> unit
    = "gluon_unix_epoll_ctl"

  external epoll_ctl_null : epoll -> flags:int -> fd:Fd.t -> unit
    = "gluon_unix_epoll_ctl_null"

  external epoll_event : events:int -> token:int64 -> event
    = "gluon_unix_epoll_event"
end

module Event = struct
  type t = event

  let token t = Token.of_int t.u64

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

  let select ?(timeout = 500_000_000L) ?(max_events = 1_000) t =
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
    let events = interests_to_epoll interest in
    let event = FFI.epoll_event ~events ~token in
    FFI.epoll_ctl t.ep Libc.epoll_ctl_add fd event

  let reregister t ~fd ~token ~interest =
    let events = interests_to_epoll interest in
    let event = FFI.epoll_event ~events ~token in
    FFI.epoll_ctl t.ep Libc.epoll_ctl_mod fd event

  let deregister t ~fd = FFI.epoll_ctl t.ep Libc.epoll_ctl_del fd
end
