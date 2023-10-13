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

  let close (conn : connection) =
    let sch = Scheduler.get_current_scheduler () in
    Io.close sch.io_tbl conn.fd

  let listen ~host ~port ~max_requests =
    let sch = Scheduler.get_current_scheduler () in
    let pid = self () in
    Logger.debug (fun f -> f "Listening on %s:%d" host port);
    Io.listen sch.io_tbl Io.{ pid; host; port; max_requests }

  let rec accept (socket : socket) =
    match[@warning "-8"] receive () with
    | Io.Socket_read fd when socket.fd = fd -> (
        match Io.accept socket with
        | Retry -> accept socket
        | Connected conn -> Ok conn)

  let rec write (conn : connection) bytes off len =
    match[@warning "-8"] receive () with
    | Io.Socket_write fd when conn.fd = fd -> (
        match Io.write conn bytes off len with
        | Retry -> write conn bytes off len
        | Wrote len -> len)

  let rec read (conn : connection) bytes off len =
    match[@warning "-8"] receive () with
    | Io.Socket_read fd when conn.fd = fd -> (
        match Io.read conn bytes off len with
        | Retry -> read conn bytes off len
        | Read len -> len)
end
