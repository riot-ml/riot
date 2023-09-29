(*
type 'msg message = 
  | Monitor of ...
  | Link of ...
  | User of 'msg
  *)

type 'msg t = {
  mailbox : 'msg Miou.Queue.t;
  is_alive : bool ref;
  fn : recv:(unit -> 'msg option) -> unit;
}

type pack = Pack : 'msg t * (unit -> 'msg option) -> pack

let make fn = { mailbox = Miou.Queue.create (); is_alive = ref true; fn }

let recv t () =
  Miou.yield ();
  match Miou.Queue.dequeue t.mailbox with
  | exception _ -> None
  | msg -> Some msg

let send t msg = Miou.Queue.enqueue t.mailbox msg

let is_alive t = !(t.is_alive)

let run (Pack (t, recv)) =
  t.fn ~recv;
  t.is_alive := false
