type unix_error = [ `Unix_error of Unix.error ]
type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

let ( let* ) = Result.bind

module Low_level = Runtime.Net.Io

module Buffer = struct
  type t = {
    inner : Cstruct.t;
    mutable capacity : int;
    mutable position : int;
        (** The current offset into [inner], must always be <= [filled] *)
    mutable filled : int;
  }

  let discard t =
    t.position <- 0;
    t.filled <- 0

  let consume t off = t.position <- Int.min (t.position + off) t.filled
  let is_empty t = t.position = 0 && t.filled = 0
  let is_full t = t.position = t.filled && not (is_empty t)
  let length t = t.capacity

  let copy ~src ~dst =
    let actual_fill = Int.min dst.capacity (src.filled - src.position) in
    Logger.trace (fun f ->
        f "copy pos=%d fill=%d dst=%d -> %d" src.position src.filled
          dst.capacity actual_fill);
    Cstruct.blit src.inner src.position dst.inner 0 actual_fill;
    dst.filled <- actual_fill;
    actual_fill

  let of_cstruct ~filled inner =
    { inner; position = 0; filled; capacity = inner.len }

  let as_cstruct t = t.inner

  let of_string str =
    let len = String.length str in
    let inner = Cstruct.of_string ~off:0 ~len str in
    of_cstruct ~filled:len inner

  let to_string t =
    Logger.trace (fun f -> f "to_string pos=%d fill=%d" t.position t.filled);
    Cstruct.to_string ~off:0 ~len:t.filled t.inner

  let with_capacity capacity = of_cstruct ~filled:0 (Cstruct.create capacity)

  let sub t ~off ~len =
    let inner = Cstruct.sub t.inner off len in
    let capacity = inner.len in
    { inner; capacity; position = 0; filled = capacity }
end

type read = Low_level.read
type write = Low_level.write

let read = Low_level.read
let write = Low_level.write

let rec single_read fd ~buf =
  match Low_level.readv fd [| Buffer.as_cstruct buf |] with
  | `Abort err -> Error (`Unix_error err)
  | `Read read -> Ok read
  | `Retry -> Runtime.syscall "single_read" `r fd @@ single_read ~buf

let rec single_write fd ~data =
  match Low_level.writev fd [| Buffer.as_cstruct data |] with
  | `Abort err -> Error (`Unix_error err)
  | `Wrote bytes -> Ok bytes
  | `Retry -> Runtime.syscall "single_write" `w fd @@ single_write ~data

let await_readable fd fn = Runtime.syscall "custom" `r fd fn
let await_writeable fd fn = Runtime.syscall "custom" `w fd fn
let await fd mode fn = Runtime.syscall "custom" mode fd fn
let copy _fd _cs = ()

module type Write = sig
  type t

  val write : t -> data:Buffer.t -> (int, [> `Closed ]) result
  val flush : t -> (unit, [> `Closed ]) result
end

module Writer = struct
  module Make (B : Write) = struct
    type t = B.t

    let write = B.write
    let flush = B.flush
  end

  type 'src write = (module Write with type t = 'src)
  type 'src t = Writer : ('src write * 'src) -> 'src t

  let of_write_src : type src. src write -> src -> src t =
   fun write src -> Writer (write, src)

  let write : type src. src t -> data:Buffer.t -> (int, [> `Closed ]) result =
   fun (Writer ((module W), src)) ~data -> W.write src ~data
end

module type Read = sig
  type t

  val read : t -> buf:Buffer.t -> (int, [> `Closed ]) result
end

module Reader = struct
  module Make (B : Read) = struct
    type t = B.t

    let read = B.read
  end

  type 'src read = (module Read with type t = 'src)
  type 'src t = Reader : ('src read * 'src) -> 'src t
  type 'src reader = 'src t

  let of_read_src : type src. src read -> src -> src t =
   fun read src -> Reader (read, src)

  let read : type src. src t -> buf:Buffer.t -> (int, [> `Closed ]) result =
   fun (Reader ((module R), src)) ~buf -> R.read src ~buf

  module Buffered = struct
    let default_buffer_size = 1_024 * 4

    type 'src t = { buf : Buffer.t; inner : 'src reader }

    let rec fill_buf t =
      Logger.trace (fun f ->
          f "fill_buf capacity=%d pos=%d fill=%d" t.buf.capacity t.buf.position
            t.buf.filled);
      if t.buf.capacity = t.buf.filled then Ok ()
      else
        let* off = read t.inner ~buf:t.buf in
        Logger.trace (fun f ->
            f "fill_buf capacity=%d pos=%d fill=%d <- read %d bytes"
              t.buf.capacity t.buf.position t.buf.filled off);
        if off = 0 then Ok ()
        else (
          t.buf.filled <- off;
          fill_buf t)

    let read t ~buf:outer =
      if t.buf.position >= t.buf.filled then (
        t.buf.position <- 0;
        t.buf.filled <- 0);
      Logger.trace (fun f ->
          f "read capacity=%d pos=%d fill=%d" t.buf.capacity t.buf.position
            t.buf.filled);
      (* If we don't have any buffered data and we're doing a massive read
         (larger than our internal buffer), bypass our internal buffer
         entirely.
      *)
      if Buffer.is_full t.buf && Buffer.length outer >= t.buf.capacity then (
        Buffer.discard t.buf;
        read t.inner ~buf:outer)
      else
        let* () = fill_buf t in
        let copied = Buffer.copy ~src:t.buf ~dst:outer in
        Buffer.consume t.buf copied;
        Ok copied

    let of_reader : type src. ?capacity:int -> src reader -> src t reader =
     fun ?(capacity = default_buffer_size) inner ->
      let t = { buf = Buffer.with_capacity capacity; inner } in
      let module BufRead = Make (struct
        type nonrec t = src t

        let read = read
      end) in
      of_read_src (module BufRead) t
  end
end
