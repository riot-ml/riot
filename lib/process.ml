type pid = int
type system_msg = Monitor_process_died of pid

type 'msg t = {
  pid : pid;
  mailbox : 'msg Miou.Queue.t;
  is_alive : bool ref;
  fn : recv:(unit -> 'msg option) -> unit;
  monitors : pid list ref;
  links : pid list ref;
}
  constraint 'msg = [> `system of system_msg ]

type pack = Pack : 'msg t * (unit -> 'msg option) -> pack

let __pids__ = Atomic.make 0
let make_pid () = Atomic.fetch_and_add __pids__ 1

let make fn =
  {
    mailbox = Miou.Queue.create ();
    is_alive = ref true;
    pid = make_pid ();
    monitors = ref [];
    links = ref [];
    fn;
  }

let recv t () =
  Miou.yield ();
  match Miou.Queue.dequeue t.mailbox with
  | exception _ -> None
  | msg -> Some msg

let signal (Pack (t, _)) (msg: system_msg) = Miou.Queue.enqueue t.mailbox (`system msg)
let send t msg = Miou.Queue.enqueue t.mailbox msg

let monitor a b =
  b.monitors := a.pid :: !(b.monitors);
  ()

let link a b =
  b.links := a.pid :: !(b.links);
  a.links := b.pid :: !(a.links);
  ()

let _is_alive t = !(t.is_alive)

let run (Pack (t, recv)) =
  t.fn ~recv;
  t.is_alive := false;
  t.pid, !(t.monitors), !(t.links)
