module type Handler = sig
  val handler : Httpaf.Reqd.t -> unit
end

module Http_protocol (H : Handler) : Server.Protocol = struct
  open Socket.Flow

  let create_flow () =
    let module S = Httpaf.Server_connection in
    let conn = S.create H.handler in

    {
      next_read_operation = (fun () -> S.next_read_operation conn);
      next_write_operation = (fun () -> S.next_write_operation conn);
      read = (fun ~buf ~len -> S.read conn buf ~off:0 ~len |> ignore);
      read_eof = (fun ~buf ~len -> S.read_eof conn buf ~off:0 ~len |> ignore);
      report_write_result = S.report_write_result conn;
      yield_reader = S.yield_reader conn;
      yield_writer = S.yield_writer conn;
    }
end

let start_link ?(host = "0.0.0.0") ?(port = 2112) ?(acceptors = 20)
    (handler : Httpaf.Reqd.t -> unit) =
  let connector =
    (module Server.Tcp_connector (Http_protocol (struct
      let handler = handler
    end)) : Server.Connector)
  in
  Server.start_link ~host ~port ~acceptors connector ()
