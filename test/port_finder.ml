open Riot

exception No_ports_available

let rec next_open_port ?(port = 10001) () =
  if port > 65_000 then raise_notrace No_ports_available
  else
    let opts =
      Net.Socket.
        {
          reuse_addr = true;
          reuse_port = false;
          backlog = 100;
          addr = Net.Addr.loopback;
        }
    in
    match Net.Socket.listen ~opts ~port () with
    | Ok socket -> (socket, port)
    | Error _ -> next_open_port ~port:(port + 1) ()
