open Core
module Tracer = Tracer
module Uid = Scheduler_uid

type t = {
  uid : Uid.t;
  rnd : Random.State.t;
  run_queue : Proc_queue.t;
  sleep_set : Proc_set.t;
  timers : Time.Timer_wheel.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
}

type io = {
  uid : Uid.t; [@warning "-69"]
  rnd : Random.State.t;
  io_tbl : Net.Io.t;
  idle_mutex : Mutex.t;
  idle_condition : Condition.t;
}

type pool = {
  mutable stop : bool;
  io_scheduler : io;
  schedulers : t list;
  processes : Proc_table.t;
  registry : Proc_registry.t;
}

val make : rnd:Random.State.t -> unit -> t
val get_current_scheduler : unit -> t
val set_current_scheduler : t -> unit
val get_current_process_pid : unit -> Pid.t
val set_current_process_pid : Pid.t -> unit
val get_random_scheduler : pool -> t

val set_timer :
  t -> float -> [ `interval | `one_off ] -> (unit -> unit) -> unit Ref.t

val add_to_run_queue : t -> Process.t -> unit
val awake_process : pool -> Process.t -> unit
val run : pool -> t -> unit -> unit

module Pool : sig
  val get_pool : unit -> pool
  val set_pool : pool -> unit
  val shutdown : pool -> unit
  val register_process : pool -> 'a -> Process.t -> unit

  val make :
    ?rnd:Random.State.t ->
    domains:int ->
    main:t ->
    unit ->
    pool * unit Domain.t list
end
