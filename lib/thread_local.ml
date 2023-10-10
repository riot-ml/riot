exception Uninitialized_thread_local of string

let make ~name =
  let value = Atomic.make None in
  let key = Domain.DLS.new_key (fun () -> Atomic.get value) in
  let get () =
    match Domain.DLS.get key with
    | Some x -> x
    | None -> raise (Uninitialized_thread_local name)
  in
  let set x = Domain.DLS.set key (Some x) in
  (get, set)
