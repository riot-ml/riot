(******************************************************************************************

  The Tls_unix below was ported from  `ocaml-tls`, its `eio` subpackage, specifically from:
    * https://github.com/mirleft/ocaml-tls/blob/main/eio/tls_eio.ml
    * https://github.com/mirleft/ocaml-tls/blob/main/eio/x509_eio.ml

  under this license:

    Copyright (c) 2014, David Kaloper and Hannes Mehnert
    All rights reserved.

    Redistribution and use in source and binary forms, with or without modification,
    are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this
      list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice, this
      list of conditions and the following disclaimer in the documentation and/or
      other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  *******************************************************************************************)

open Logger.Make (struct
  let namespace = [ "riot"; "net"; "ssl" ]
end)

module IO = Io

let ( let* ) = Result.bind

type 'src t = {
  writer : 'src IO.Writer.t;
  reader : 'src IO.Reader.t;
  mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
  mutable linger : Cstruct.t option;
  recv_buf : Cstruct.t;
}

exception Tls_alert of Tls.Packet.alert_type
exception Tls_failure of Tls.Engine.failure

module Tls_unix = struct
  exception Read_error of Io.io_error
  exception Write_error of Io.io_error

  let err_to_str err = Format.asprintf "%a" Io.pp_err err

  let read_t t cs =
    let buf = IO.Bytes.with_capacity (Cstruct.length cs) in
    match IO.read t.reader ~buf with
    | Ok len ->
        trace (fun f -> f "read_t: %d/%d" len (Cstruct.length cs));
        Cstruct.blit_from_bytes buf 0 cs 0 len;
        len
    | Error (`Closed | `Eof) ->
        trace (fun f -> f "read_t: 0/%d" (Cstruct.length cs));
        raise End_of_file
    | Error err ->
        trace (fun f -> f "read_t: error: %s" (err_to_str err));
        let exn = Read_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let write_t t cs =
    let bufs = IO.Iovec.from_cstruct cs in
    match IO.write_owned_vectored t.writer ~bufs with
    | Ok bytes -> trace (fun f -> f "write_t: %d/%d" bytes (Cstruct.length cs))
    | Error err ->
        trace (fun f -> f "write_t: error: %s" (err_to_str err));
        let exn = Write_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let try_write_t t cs =
    try write_t t cs with _ -> trace (fun f -> f "try_write_t failed")

  let rec read_react t =
    trace (fun f -> f "tls.read_react");
    let handle tls cs =
      match Tls.Engine.handle_tls tls cs with
      | Ok (state', `Response resp, `Data data) ->
          trace (fun f -> f "tls.read_react->ok");
          let state' =
            match state' with
            | `Ok tls -> `Active tls
            | `Eof -> `Eof
            | `Alert a ->
                trace (fun f -> f "tls.read_react->alert");
                `Error (Tls_alert a)
          in
          t.state <- state';
          Option.iter (try_write_t t) resp;
          data
      | Error (alert, `Response resp) ->
          trace (fun f -> f "tls.read_react->error");
          t.state <- `Error (Tls_failure alert);
          write_t t resp;
          read_react t
    in

    match t.state with
    | `Error e -> raise e
    | `Eof -> raise End_of_file
    | `Active _ -> (
        let n = read_t t t.recv_buf in
        match (t.state, n) with
        | `Active tls, n -> handle tls (Cstruct.sub t.recv_buf 0 n)
        | `Error e, _ -> raise e
        | `Eof, _ -> raise End_of_file)

  let rec single_read t cs =
    let writeout res =
      let open Cstruct in
      let rlen = length res in
      let n = min (length cs) rlen in
      blit res 0 cs 0 n;
      t.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None);
      n
    in

    match t.linger with
    | Some res -> writeout res
    | None -> (
        match read_react t with
        | None -> single_read t cs
        | Some res -> writeout res)

  exception Tls_socket_closed

  let writev t css =
    match t.state with
    | `Error err ->
        trace (fun f -> f "writev: failed");
        raise err
    | `Eof -> raise Tls_socket_closed
    | `Active tls -> (
        match Tls.Engine.send_application_data tls css with
        | Some (tls, tlsdata) ->
            t.state <- `Active tls;
            write_t t tlsdata
        | None -> invalid_arg "tls: write: socket not ready")

  let single_write t cs =
    writev t [ cs ];
    let written = Cstruct.lenv [ cs ] in
    Ok written

  let rec drain_handshake t =
    let push_linger t mcs =
      match (mcs, t.linger) with
      | None, _ -> ()
      | scs, None -> t.linger <- scs
      | Some cs, Some l -> t.linger <- Some (Cstruct.append l cs)
    in
    match t.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> t
    | _ ->
        let cs = read_react t in
        push_linger t cs;
        drain_handshake t

  let epoch t =
    match t.state with
    | `Active tls ->
        Tls.Engine.epoch tls |> Result.map_error (fun () -> `No_session_data)
    | _ -> Error `Inactive_tls_engine

  let make_client ?host ~reader ~writer config =
    let config' =
      match host with
      | None -> config
      | Some host -> Tls.Config.peer config host
    in
    let t =
      {
        state = `Eof;
        writer;
        reader;
        linger = None;
        recv_buf = Cstruct.create 4_096;
      }
    in
    let tls, init = Tls.Engine.client config' in
    let t = { t with state = `Active tls } in
    write_t t init;
    drain_handshake t

  let make_server ~reader ~writer config =
    let t =
      {
        state = `Active (Tls.Engine.server config);
        writer;
        reader;
        linger = None;
        recv_buf = Cstruct.create 4_096;
      }
    in
    drain_handshake t

  let to_reader : type src. src t -> src t IO.Reader.t =
   fun t ->
    let module Read = struct
      type nonrec t = src t

      let read t ~buf =
        let cs = Cstruct.create 1024 in
        let len =
          match single_read t cs with exception End_of_file -> 0 | len -> len
        in
        let bufs = IO.Iovec.from_cstruct cs in
        let* _ =
          if IO.Iovec.length bufs > 0 then
            let writer = IO.Bytes.to_writer buf in
            IO.write_owned_vectored writer ~bufs
          else Ok 0
        in
        Ok len

      let read_vectored _t ~bufs:_ = Ok 0
    end in
    IO.Reader.of_read_src (module Read) t

  let to_writer : type src. src t -> src t IO.Writer.t =
   fun t ->
    let module Write = struct
      type nonrec t = src t

      let write t ~buf = single_write t (Cstruct.of_bytes buf)

      let write_owned_vectored t ~bufs =
        single_write t (IO.Iovec.into_cstruct bufs)

      let flush _t = Ok ()
    end in
    IO.Writer.of_write_src (module Write) t
end

let negotiated_protocol t =
  let* epoch = Tls_unix.epoch t in
  Ok Tls.Core.(epoch.alpn_protocol)

let to_reader = Tls_unix.to_reader
let to_writer = Tls_unix.to_writer

let of_server_socket ?read_timeout ?send_timeout
    ?(config = Tls.Config.server ()) sock =
  let reader, writer =
    Net.Tcp_stream.
      ( to_reader ?timeout:read_timeout sock,
        to_writer ?timeout:send_timeout sock )
  in
  Tls_unix.make_server ~reader ~writer config

let of_client_socket ?read_timeout ?send_timeout ?host ~config sock =
  let reader, writer =
    Net.Tcp_stream.
      ( to_reader ?timeout:read_timeout sock,
        to_writer ?timeout:send_timeout sock )
  in
  Tls_unix.make_client ?host ~reader ~writer config
