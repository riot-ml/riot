type socket = {
  fd : Unix.file_descr;
  addr : Unix.sockaddr;
  port : int;
  max_requests : int;
}

module Socket_table = Hashtbl.Make(struct

end)

type t = {
  read_fds: (socket, Pid.t) Hashtbl.t;
  write_fds: (socket, Pid.t) Hashtbl.t;
  except_fds: (socket, Pid.t) Hashtbl.t;
}

let select table =
  let read = table.read_dfs  |> Hashtbl.to_seq_keys |> Seq.map (fun {fd;_} -> fd) |> List.of_seq in 
  let write = table.write_fds |> Hashtbl.to_seq_keys |> Seq.map (fun {fd;_} -> fd) |> List.of_seq in 
  let except = table.write_fds |> Hashtbl.to_seq_keys |> Seq.map (fun {fd;_} -> fd) |> List.of_seq in 
  UnixLabels.select
  ~read ~write
