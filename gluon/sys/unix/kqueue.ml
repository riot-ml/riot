open Gluon_common

type kevent
type kqueue = Fd.t

module FFI : sig
  val fcntl : kqueue -> int -> int -> int io_result
  val kevent_create : kqueue -> filter:int -> flags:int -> token:int -> kevent
  val kevent_register : kqueue -> kevent array -> int array -> unit io_result
  val kqueue : unit -> kqueue io_result

  val kevent :
    max_events:int -> timeout:int64 -> kqueue -> kevent array io_result

  val kevent_udata : kevent -> int
  val kevent_filter : kevent -> int
  val kevent_flags : kevent -> int
end = struct
  external gluon_unix_kevent :
    max_events:int -> timeout:int64 -> kqueue -> kevent array
    = "gluon_unix_kevent"

  let kevent ~max_events ~timeout kq =
    syscall @@ fun () -> gluon_unix_kevent ~max_events ~timeout kq

  external gluon_unix_kqueue : unit -> kqueue = "gluon_unix_kqueue"

  let kqueue () = syscall @@ fun () -> gluon_unix_kqueue ()

  external gluon_unix_fcntl : Fd.t -> cmd:int -> arg:int -> int
    = "gluon_unix_fcntl"

  let fcntl fd cmd arg = syscall @@ fun () -> gluon_unix_fcntl fd ~cmd ~arg

  external gluon_unix_kevent_register :
    kqueue -> kevent array -> int array -> unit = "gluon_unix_kevent_register"

  let kevent_register fd changes ignored_errors =
    syscall @@ fun () -> gluon_unix_kevent_register fd changes ignored_errors

  external gluon_unix_kevent_create :
    Fd.t -> filter:int -> flags:int -> token:int -> kevent
    = "gluon_unix_kevent_create"

  let kevent_create kq ~filter ~flags ~token =
    gluon_unix_kevent_create kq ~filter ~flags ~token

  external gluon_unix_kevent_udata : kevent -> int = "gluon_unix_kevent_udata"

  let kevent_udata kev = gluon_unix_kevent_udata kev

  external gluon_unix_kevent_filter : kevent -> int = "gluon_unix_kevent_filter"

  let kevent_filter kev = gluon_unix_kevent_filter kev

  external gluon_unix_kevent_flags : kevent -> int = "gluon_unix_kevent_flags"

  let kevent_flags kev = gluon_unix_kevent_flags kev
end

module Event = struct
  type t = kevent

  let filter t = FFI.kevent_filter t
  let flags t = FFI.kevent_flags t
  let token t = Token.of_int (FFI.kevent_udata t)
  let is_readable t = filter t = Libc.evfilt_read
  let is_writable t = filter t = Libc.evfilt_write
  let is_error t = flags t land Libc.ev_error != 0
  let is_read_closed t = is_readable t && flags t land Libc.ev_eof != 0
  let is_write_closed t = is_writable t && flags t land Libc.ev_eof != 0
  let is_priority _t = false
end

module Selector = struct
  let name = "kqueue"

  type t = { kq : kqueue }

  let make () =
    let* kq = FFI.kqueue () in
    let* _ = FFI.(fcntl kq Libc.f_setfd Libc.f_dupfd_cloexec) in
    Ok { kq }

  let select ?(timeout = 500_000_000L) ?(max_events = 100) t =
    let* events = FFI.kevent ~timeout ~max_events t.kq in
    let events = Array.to_list events in
    let events = List.map (Gluon_events.Event.make (module Event)) events in
    Ok events

  let register t ~fd ~token ~interest =
    let tok = token in
    let token = Token.to_int token in
    let flags = Libc.(ev_clear lor ev_receipt lor ev_add) in
    let changes = ref [] in

    if Interest.is_writable interest then (
      Format.printf "%a registering writeable interest for %a\r\n%!" Token.pp tok Fd.pp fd;
      let kevent =
        FFI.kevent_create fd ~filter:Libc.evfilt_write ~flags ~token
      in
      changes := kevent :: !changes);

    if Interest.is_readable interest then (
      Format.printf "%a registering readable interest for %a\r\n%!" Token.pp tok Fd.pp fd;
      let kevent =
        FFI.kevent_create fd ~filter:Libc.evfilt_read ~flags ~token
      in
      changes := kevent :: !changes);

    let changes = Array.of_list !changes in
    Format.printf "%a registering %a\r\n%!" Token.pp tok Fd.pp fd;
    FFI.kevent_register t.kq changes [| Libc.epipe |]

  let reregister t ~fd ~token ~interest =
    let token = Token.to_int token in
    let flags = Libc.(ev_clear lor ev_receipt) in

    let write_flags =
      if Interest.is_writable interest then Libc.(flags lor ev_add)
      else Libc.(flags lor ev_delete)
    in

    let read_flags =
      if Interest.is_readable interest then Libc.(flags lor ev_add)
      else Libc.(flags lor ev_delete)
    in

    let changes =
      [|
        FFI.kevent_create fd ~filter:Libc.evfilt_write ~flags:write_flags ~token;
        FFI.kevent_create fd ~filter:Libc.evfilt_read ~flags:read_flags ~token;
      |]
    in

    Format.printf "reregistering %a\r\n%!" Fd.pp fd;
    FFI.kevent_register t.kq changes Libc.[| epipe; enoent |]

  let deregister t ~fd =
    let flags = Libc.(ev_delete lor ev_receipt) in
    let changes =
      [|
        FFI.kevent_create fd ~filter:Libc.evfilt_write ~flags ~token:0;
        FFI.kevent_create fd ~filter:Libc.evfilt_read ~flags ~token:0;
      |]
    in
    Format.printf "deregistering %a\r\n%!" Fd.pp fd;
    FFI.kevent_register t.kq changes Libc.[| enoent |]
end
