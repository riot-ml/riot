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

module IO = Rio

let ( let* ) = Result.bind

type 'src t = {
  writer : 'src IO.Writer.t;
  reader : 'src IO.Reader.t;
  mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
  mutable linger : string option;
  recv_buf : bytes;
}

exception Tls_alert of Tls.Packet.alert_type
exception Tls_failure of Tls.Engine.failure

module Tls_unix = struct
  exception Read_error of Rio.io_error
  exception Write_error of Rio.io_error

  let err_to_str err = Format.asprintf "%a" Rio.pp_err err

  let read_t t dst =
    let src = IO.Bytes.with_capacity (Bytes.length dst) in
    match IO.read t.reader src with
    | Ok len ->
        trace (fun f -> f "read_t: %d/%d" len (Bytes.length dst));
        BytesLabels.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len;
        len
    | Error (`Closed | `Eof) ->
        trace (fun f -> f "read_t: 0/%d" (Bytes.length dst));
        raise End_of_file
    | Error err ->
        trace (fun f -> f "read_t: error: %s" (err_to_str err));
        let exn = Read_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let write_t t data =
    let bufs = IO.Iovec.from_string data in
    match IO.write_owned_vectored t.writer ~bufs with
    | Ok bytes -> trace (fun f -> f "write_t: %d/%d" bytes (String.length data))
    | Error err ->
        trace (fun f -> f "write_t: error: %s" (err_to_str err));
        let exn = Write_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let try_write_t t cs =
    try write_t t cs with _ -> trace (fun f -> f "try_write_t failed")

  let inject_state tls = function
    | `Active _ -> `Active tls
    | `Eof -> `Eof
    | `Error _ as e -> e

  let rec read_react t =
    trace (fun f -> f "tls.read_react");
    let handle tls cs =
      match Tls.Engine.handle_tls tls cs with
      | Ok (state', eof, `Response resp, `Data data) ->
          trace (fun f -> f "tls.read_react->ok");
          let state' =
            match eof with
            | Some `Eof -> `Eof
            | _ -> inject_state state' t.state
          in
          t.state <- state';
          Option.iter (try_write_t t) resp;
          data
      | Error (fail, `Response resp) ->
          let state' =
            match fail with
            | `Alert a ->
                trace (fun f -> f "tls.read_react->alert");
                `Error (Tls_alert a)
            | f ->
                trace (fun f -> f "tls.read_react->error");
                `Error (Tls_failure f)
          in
          t.state <- state';
          write_t t resp;
          read_react t
    in

    match t.state with
    | `Error e -> raise e
    | `Eof -> raise End_of_file
    | `Active _ -> (
        let n = read_t t t.recv_buf in
        match (t.state, n) with
        | `Active tls, n ->
            handle tls (String.of_bytes (Bytes.sub t.recv_buf 0 n))
        | `Error e, _ -> raise e
        | `Eof, _ -> raise End_of_file)

  let rec single_read t (dst : bytes) =
    let writeout (data : string) =
      let rlen = String.length data in
      let n = min (Bytes.length dst) rlen in
      StringLabels.blit ~src:data ~src_pos:0 ~dst ~dst_pos:0 ~len:n;
      t.linger <-
        (if n < rlen then Some (String.sub data n (rlen - n)) else None);
      n
    in

    match t.linger with
    | Some res -> writeout res
    | None -> (
        match read_react t with
        | None -> single_read t dst
        | Some res -> writeout res)

  exception Tls_socket_closed

  let writev t data =
    match t.state with
    | `Error err ->
        trace (fun f -> f "writev: failed");
        raise err
    | `Eof -> raise Tls_socket_closed
    | `Active tls -> (
        match Tls.Engine.send_application_data tls data with
        | Some (tls, tlsdata) ->
            t.state <- `Active tls;
            write_t t tlsdata
        | None -> invalid_arg "tls: write: socket not ready")

  let single_write t src =
    writev t [ src ];
    let written = String.length src in
    Ok written

  let rec drain_handshake t =
    let push_linger t mcs =
      match (mcs, t.linger) with
      | None, _ -> ()
      | scs, None -> t.linger <- scs
      | Some cs, Some l -> t.linger <- Some (l ^ cs)
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
        recv_buf = Bytes.create 4_096;
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
        recv_buf = Bytes.create 4_096;
      }
    in
    drain_handshake t

  let to_reader : type src. src t -> src t IO.Reader.t =
   fun t ->
    let module Read = struct
      type nonrec t = src t

      let read t ?timeout:_ dst =
        match single_read t dst with
        | exception End_of_file -> Ok 0
        | len -> Ok len

      let read_vectored _t _bufs = Ok 0
    end in
    IO.Reader.of_read_src (module Read) t

  let to_writer : type src. src t -> src t IO.Writer.t =
   fun t ->
    let module Write = struct
      type nonrec t = src t

      let write t ~buf = single_write t buf

      (* TODO: This seems like not what we want *)
      let write_owned_vectored t ~bufs =
        single_write t (IO.Iovec.into_string bufs)
      (* single_write t bufs *)

      let flush _t = Ok ()
    end in
    IO.Writer.of_write_src (module Write) t
end

let negotiated_protocol t =
  let* epoch = Tls_unix.epoch t in
  Ok Tls.Core.(epoch.alpn_protocol)

let to_reader = Tls_unix.to_reader
let to_writer = Tls_unix.to_writer

let of_server_socket ?read_timeout ?send_timeout ~config sock =
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
