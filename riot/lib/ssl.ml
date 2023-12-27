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

module IO = Io

let ( let* ) = Result.bind

type 'src t = {
  writer : 'src IO.Writer.t;
  reader : 'src IO.Reader.t;
  mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
  mutable linger : Cstruct.t option;
  recv_buf : Cstruct.t;
}

module Tls_unix = struct
  exception Tls_alert of Tls.Packet.alert_type
  exception Tls_failure of Tls.Engine.failure
  exception Read_error of [ `Closed | `Eof | `Unix_error of Unix.error ]
  exception Write_error of [ `Closed | `Eof | `Unix_error of Unix.error ]

  let err_to_str err =
    match err with
    | `Closed -> "closed"
    | `Eof -> "eof"
    | `Unix_error err -> Unix.error_message err

  let read_t t cs =
    match IO.Reader.read t.reader ~buf:(IO.Buffer.of_cstruct cs) with
    | Ok n ->
        Logger.debug (fun f -> f "read_t: %d/%d" n (Cstruct.length cs));
        n
    | Error (`Closed | `Eof) ->
        Logger.debug (fun f -> f "read_t: 0/%d" (Cstruct.length cs));
        raise End_of_file
    | Error err ->
        Logger.debug (fun f -> f "read_t: error: %s" (err_to_str err));
        let exn = Read_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let write_t t cs =
    match IO.write_all t.writer ~data:(IO.Buffer.of_cstruct cs) with
    | Ok bytes ->
        Logger.debug (fun f -> f "write_t: %d/%d" bytes (Cstruct.length cs))
    | Error err ->
        Logger.debug (fun f -> f "write_t: error: %s" (err_to_str err));
        let exn = Write_error err in
        (match t.state with
        | `Error _ | `Eof -> ()
        | `Active _ -> t.state <- `Error exn);
        raise exn

  let try_write_t t cs =
    try write_t t cs with _ -> Logger.debug (fun f -> f "try_write_t failed")

  let rec read_react t =
    Logger.debug (fun f -> f "tls.read_react");
    let handle tls cs =
      match Tls.Engine.handle_tls tls cs with
      | Ok (state', `Response resp, `Data data) ->
          Logger.debug (fun f -> f "tls.read_react->ok");
          let state' =
            match state' with
            | `Ok tls -> `Active tls
            | `Eof -> `Eof
            | `Alert a ->
                Logger.debug (fun f -> f "tls.read_react->alert");
                `Error (Tls_alert a)
          in
          t.state <- state';
          Option.iter (try_write_t t) resp;
          data
      | Error (alert, `Response resp) ->
          Logger.debug (fun f -> f "tls.read_react->error");
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

  let rec single_read t buf =
    let writeout res =
      let open Cstruct in
      let rlen = length res in
      let n = min (length buf) rlen in
      blit res 0 buf 0 n;
      t.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None);
      n
    in

    match t.linger with
    | Some res -> writeout res
    | None -> (
        match read_react t with
        | None -> single_read t buf
        | Some res -> writeout res)

  exception Tls_socket_closed

  let writev t css =
    match t.state with
    | `Error err ->
        Logger.debug (fun f -> f "writev: failed");
        raise err
    | `Eof -> raise Tls_socket_closed
    | `Active tls -> (
        match Tls.Engine.send_application_data tls css with
        | Some (tls, tlsdata) ->
            t.state <- `Active tls;
            write_t t tlsdata
        | None -> invalid_arg "tls: write: socket not ready")

  let single_write t ~data =
    let cs = IO.Buffer.as_cstruct data in
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

  let make ?host ~reader ~writer config =
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

  let to_reader : type src. src t -> src t IO.Reader.t =
   fun t ->
    let module Read = IO.Reader.Make (struct
      type nonrec t = src t

      let read t ~buf =
        let cs = IO.Buffer.as_cstruct buf in
        let len =
          match single_read t cs with exception End_of_file -> 0 | len -> len
        in
        IO.Buffer.set_filled buf ~filled:len;
        Ok len
    end) in
    IO.Reader.of_read_src (module Read) t

  let to_writer : type src. src t -> src t IO.Writer.t =
   fun t ->
    let module Write = IO.Writer.Make (struct
      type nonrec t = src t

      let write = single_write
      let flush _t = Ok ()
    end) in
    IO.Writer.of_write_src (module Write) t
end

let of_socket ?host ~auth sock =
  let reader, writer = (Net.Socket.to_reader sock, Net.Socket.to_writer sock) in
  let tls = Tls_unix.make ?host ~reader ~writer auth in
  (Tls_unix.to_reader tls, Tls_unix.to_writer tls)
