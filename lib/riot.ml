module Gen_server = Gen_server
module Logger = Logger
module Message = Message
module Pid = Pid
module Process = Process
module Ref = Ref
module Supervisor = Supervisor

module type Logger = Logger.Intf

type ('a, 'b) logger_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

include Riot_api
(** Public API *)

let shutdown () =
  let pool = _get_pool () in
  Scheduler.Pool.shutdown pool

let run ?(rnd = Random.State.make_self_init ())
    ?(workers = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Pid.reset ();
  Scheduler.Uid.reset ();

  let sch0 = Scheduler.make ~rnd () in
  let pool, domains = Scheduler.Pool.make ~main:sch0 ~domains:workers () in

  Scheduler.set_current_scheduler sch0;
  Scheduler.Pool.set_pool pool;

  let _pid = _spawn pool sch0 main in
  Scheduler.run pool sch0 ();

  Logs.debug (fun f -> f "Riot runtime shutting down...");
  List.iter Stdlib.Domain.join domains;
  Logs.debug (fun f -> f "Riot runtime shutdown");
  ()

module Unix = struct
  type socket = Io.socket
  type connection = Io.connection

  module Logger = Logger.Make (struct
    let namespace = [ "riot"; "unix" ]
  end)

  let close_socket (socket : socket) =
    let sch = Scheduler.get_current_scheduler () in
    Logger.trace (fun f -> f "Closing socket");
    Io.close sch.io_tbl socket.fd

  let close_connection (conn : connection) =
    let sch = Scheduler.get_current_scheduler () in
    Logger.trace (fun f -> f "Closing connection");
    Io.close sch.io_tbl conn.fd

  let listen ~host ~port ~max_requests =
    let sch = Scheduler.get_current_scheduler () in
    let pid = self () in
    Logger.trace (fun f -> f "Listening on %s:%d" host port);
    Io.listen sch.io_tbl Io.{ pid; host; port; max_requests }

  let accept (socket : socket) =
    let sch = Scheduler.get_current_scheduler () in
    let pid = self () in
    Io.register_read sch.io_tbl socket.fd pid;
    let rec do_accept () =
      match receive () with
      | Io.Socket_read fd when socket.fd = fd -> (
          Logger.trace (fun f -> f "Accepting connections...");
          match Io.accept socket with
          | Abort ->
              close_socket socket;
              Error `Connection_aborted
          | Retry -> do_accept ()
          | Connected conn -> Ok conn)
      | _ -> do_accept ()
    in
    do_accept ()

  let write (conn : connection) bytes off len =
    let sch = Scheduler.get_current_scheduler () in
    let pid = self () in
    Io.register_write sch.io_tbl conn.fd pid;
    let rec do_write () =
      match receive () with
      | Io.Socket_write fd when conn.fd = fd -> (
          Logger.trace (fun f -> f "writing");
          match Io.write conn bytes off len with
          | Abort ->
              close_connection conn;
              0
          | Retry -> do_write ()
          | Wrote len -> len)
      | _ -> do_write ()
    in
    do_write ()

  let read (conn : connection) bytes off len =
    let sch = Scheduler.get_current_scheduler () in
    let pid = self () in
    Io.register_read sch.io_tbl conn.fd pid;
    let rec do_read () =
      match receive () with
      | Io.Socket_read fd when conn.fd = fd -> (
          Logger.trace (fun f -> f "reading");
          match Io.read conn bytes off len with
          | Abort ->
              close_connection conn;
              0
          | Retry -> do_read ()
          | Read len -> len)
      | _ -> do_read ()
    in
    do_read ()
end
