module Logger = Logger.Make (struct
  let namespace = [ "riot"; "io" ]
end)

type unix_error = [ `Process_down | `Timeout | `Unix_error of Unix.error ]
type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

let pp_err fmt = function
  | `Timeout -> Format.fprintf fmt "Timeout"
  | `Process_down -> Format.fprintf fmt "Process_down"
  | `System_limit -> Format.fprintf fmt "System_limit"
  | `Closed -> Format.fprintf fmt "Closed"
  | `Unix_error err ->
      Format.fprintf fmt "Unix_error(%s)" (Unix.error_message err)

let ( let* ) = Result.bind

module Low_level = Runtime.Net.Io

module Buffer = struct
  type buffer =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    inner : Cstruct.t;
    mutable capacity : int;
    mutable position : int;
        (** The current offset into [inner], must always be <= [filled] *)
    mutable filled : int;
  }

  let empty = { inner = Cstruct.empty; position = 0; filled = 0; capacity = 0 }

  let concat a b =
    let inner = Cstruct.concat [ a.inner; b.inner ] in
    { a with inner; capacity = a.capacity + b.capacity }

  let position t = t.position
  let filled t = t.filled
  let set_filled t ~filled = t.filled <- Int.min filled t.capacity
  let consume t off = t.position <- Int.min (t.position + off) t.filled
  let is_empty t = t.position = 0 && t.filled = 0
  let is_full t = t.position = t.filled && not (is_empty t)
  let length t = t.capacity

  let discard t =
    t.position <- 0;
    t.filled <- 0

  let copy ~src ~dst =
    let actual_fill = Int.min dst.capacity (src.filled - src.position) in
    Logger.trace (fun f ->
        f "copy pos=%d fill=%d dst=%d -> %d" src.position src.filled
          dst.capacity actual_fill);
    Cstruct.blit src.inner src.position dst.inner 0 actual_fill;
    dst.filled <- actual_fill;
    actual_fill

  let of_cstruct ?filled inner =
    let filled = Option.value ~default:(Cstruct.length inner) filled in
    { inner; position = 0; filled; capacity = inner.len }

  let as_cstruct t =
    Cstruct.of_bigarray ~off:t.position ~len:(t.capacity - t.position)
      t.inner.buffer

  let of_string str =
    let len = String.length str in
    let inner = Cstruct.of_string ~off:0 ~len str in
    of_cstruct ~filled:len inner

  let to_string t =
    Logger.trace (fun f -> f "to_string pos=%d fill=%d" t.position t.filled);
    Cstruct.to_string ~off:0 ~len:t.filled t.inner

  let with_capacity capacity = of_cstruct ~filled:0 (Cstruct.create capacity)

  let sub ?(off = 0) ?len t =
    let len = Option.value ~default:(length t - off) len in
    let inner = Cstruct.sub t.inner off len in
    let capacity = inner.len in
    { inner; capacity; position = 0; filled = capacity }

  let split ~on t =
    let splits = ref [] in
    let window_size = String.length on in
    let current = ref t in
    let exception Done in
    try
      let last_off = ref 0 in
      for _ = 0 to t.filled - 1 - window_size do
        let off = !last_off in
        let window =
          if off + window_size < length !current then
            sub ~off ~len:window_size !current
          else (
            (* save everything up to the window *)
            splits := !current :: !splits;
            raise_notrace Done)
        in
        let win_str = to_string window in
        let matches = String.equal win_str on in
        (* if we find the window is exactly what we're searching for, we have a
           split point *)
        if matches then (
          let split = sub ~off:0 ~len:off !current in
          (* save everything up to the window *)
          splits := split :: !splits;
          (* move our current to after the window *)
          let rest = sub ~off:(off + window_size) !current in
          current := rest;
          last_off := 0)
        else last_off := off + 1
      done;
      !splits |> List.rev
    with Done -> !splits |> List.rev
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
   fun (Reader ((module R), src)) ~buf ->
    Logger.trace (fun f -> f " IO.Reader.read");
    match R.read src ~buf with Ok len -> Ok len | Error err -> Error err

  let empty =
    let module EmptyRead = struct
      type t = unit

      let read () ~buf:_ = Ok 0
    end in
    of_read_src (module EmptyRead) ()

  module Buffered = struct
    let default_buffer_size = 1_024 * 4

    type 'src t = { buf : Buffer.t; inner : 'src reader }

    let to_buffer t =
      t.buf.position <- 0;
      t.buf

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

    let of_buffer buf =
      let t = { buf; inner = empty } in
      let module BufRead = Make (struct
        type nonrec t = unit t

        let read = read
      end) in
      of_read_src (module BufRead) t
  end

  let of_buffer buf = Buffered.of_buffer buf
end

let write_all dst ~data =
  let rec write_all data n =
    Logger.trace (fun f ->
        f "io.write_all: written=%d buf_size=%d" n (Buffer.length data));
    if not (Buffer.is_full data) then (
      let* written = Writer.write dst ~data in
      Buffer.consume data written;
      write_all data (n + written))
    else Ok n
  in
  write_all data 0

let default_copy_buffer () = Buffer.with_capacity (1024 * 1024 * 4)

let copy ?(buf = default_copy_buffer ()) src dst =
  let rec read_all copied =
    match Reader.read src ~buf with
    | Ok 0 -> Ok copied
    | Ok len ->
        let data = Buffer.sub ~len buf in
        let* _written = write_all dst ~data in
        read_all (copied + len)
    | Error err -> Error err
  in
  read_all 0

let copy_buffered Reader.(Reader (_, src)) dst =
  write_all dst ~data:(Reader.Buffered.to_buffer src)
