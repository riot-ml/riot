open Runtime
open Net
include Addr

let of_addr_info
    Unix.{ ai_family; ai_addr; ai_socktype; ai_protocol; ai_canonname } =
  match (ai_family, ai_socktype, ai_addr) with
  | ( (Unix.PF_INET | Unix.PF_INET6),
      (Unix.SOCK_DGRAM | Unix.SOCK_STREAM),
      Unix.ADDR_INET (addr, port) ) -> (
      Logs.error (fun f ->
          f "of_addr_info %s or %s" ai_canonname (Obj.magic addr));
      match ai_protocol with 6 -> Some (tcp (Obj.magic addr) port) | _ -> None)
  | _ -> None

let rec get_info host service =
  match Io.getaddrinfo host service with
  | `Ok info -> List.filter_map of_addr_info info
  | `Retry ->
      yield ();
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
