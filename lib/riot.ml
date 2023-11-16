module Gen_server = Gen_server
module Logger = Logger
module Telemetry = Telemetry_app
module Message = Message

module Net = struct
  module Addr = struct
    include Addr

    let of_addr_info
        Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; ai_canonname } =
      match (ai_family, ai_socktype, ai_addr) with
      | ( (Unix.PF_INET | Unix.PF_INET6),
          (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
          Unix.ADDR_INET (addr, port) ) -> (
          Logs.error (fun f ->
              f "of_addr_info %s or %s" ai_canonname (Obj.magic addr));
          match ai_protocol with
          | 6 -> Some (tcp (Obj.magic addr) port)
          | _ -> None)
      | _ -> None

    let rec get_info host service =
      match Io.getaddrinfo host service with
      | `Ok info -> List.filter_map of_addr_info info
      | `Retry ->
          Riot_api.yield ();
          get_info host service
      | `Abort _err -> failwith "getaddrinfo failed"

    let of_uri uri =
      let port =
        match Uri.port uri with
        | Some port -> Int.to_string port
        | _ -> Uri.scheme uri |> Option.value ~default:"http"
      in
      let host = Uri.host_with_default ~default:"0.0.0.0" uri in
      Logs.error (fun f -> f "host: %s port: %s" host port);
      match get_info host port with ip :: _ -> Some ip | [] -> None

    let get_info (`Tcp (host, port)) = get_info host (Int.to_string port)
  end

  module Socket = Socket
  include Net
end

module Pid = Pid
module Process = Process
module Ref = Ref
module Supervisor = Supervisor
module Queue = Lf_queue

module type Logger = Logger.Intf

type ('a, 'b) logger_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

include Riot_api
(** Public API *)

let shutdown () =
  Logger.warn (fun f -> f "RIOT IS SHUTTING DOWN!");
  let pool = _get_pool () in
  Scheduler.Pool.shutdown pool

let run ?(rnd = Random.State.make_self_init ())
    ?(workers = max 0 (Stdlib.Domain.recommended_domain_count () - 1)) main =
  Logs.debug (fun f -> f "Initializing Riot runtime...");
  Printexc.record_backtrace true;
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
  Logs.debug (fun f -> f "Riot runtime shutdown")

module Application = struct
  module type Intf = sig
    val name : string

    val start :
      unit ->
      ( Pid.t,
        ([> `Application_error of string | `Supervisor_error ] as 'err) )
      result
  end
end

let start ?rnd ?workers ~apps () =
  run ?rnd ?workers @@ fun () ->
  let pids =
    List.fold_left
      (fun acc (module App : Application.Intf) ->
        match (acc, App.start ()) with
        | Ok acc, Ok pid -> Ok (pid :: acc)
        | Ok _, Error error ->
            Logs.error (fun f ->
                f "Could not start application %s due to %s" App.name
                  (Marshal.to_string error []));
            Error ()
        | Error (), _ -> acc)
      (Ok []) apps
    |> Result.get_ok
  in
  wait_pids pids
