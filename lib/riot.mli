module Logs = Logs

module Pid : sig
  type t

  val zero : t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Message : sig
  type select_marker = Take | Skip
  type monitor = Process_down of Pid.t
  type t = ..
  type t += Exit_signal | Monitor of monitor
end

module Process : sig
  type t
end

type exit_reason =
  | Normal
  | Exit_signal
  | Timeout_value
  | Bad_link
  | Exception of exn

type signal = Link | Unlink | Exit | Monitor | Demonitor | Message

val yield : unit -> unit
val self : unit -> Pid.t
val exit : Pid.t -> exit_reason -> unit
val send : Pid.t -> Message.t -> unit
val spawn : (unit -> unit) -> Pid.t

exception Link_no_process of Pid.t

val link : Pid.t -> unit
val monitor : Pid.t -> Pid.t -> unit
val processes : unit -> (Pid.t * Process.t) Seq.t
val is_process_alive : Pid.t -> bool
val random : unit -> Random.State.t
val receive : ?select:(Message.t -> Message.select_marker) -> unit -> Message.t
val shutdown : unit -> unit
val run : ?rnd:Random.State.t -> ?workers:int -> (unit -> unit) -> unit

module Supervisor : sig
  type child_spec

  type strategy =
    | One_for_one
        (** If one child process terminates and is to be restarted, only that
      child process is affected. This is the default restart strategy.*)
    | One_for_all
        (** If one child process terminates and is to be restarted, all other
      child processes are terminated and then all child processes are
      restarted. *)
    | Rest_for_one
        (**  If one child process terminates and is to be restarted, the 'rest'
       of the child processes (that is, the child processes after the
       terminated child process in the start order) are terminated. Then
       the terminated child process and all child processes after it are
       restarted. *)
    | Simple_one_for_one
        (** A simplified one_for_one supervisor, where all child processes are
      dynamically added instances of the same process type, that is,
      running the same code. *)

  val child_spec :
    start_link:('state -> (Pid.t, exn) result) -> 'state -> child_spec

  val start_link :
    ?strategy:strategy ->
    ?restart_intensity:int ->
    ?restart_period:int ->
    child_specs:child_spec list ->
    unit ->
    (Pid.t, [> `Supervisor_error ]) result
end
