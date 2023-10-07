module Logs = Logs

module Pid : sig
  type t

  val zero : t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Message : sig
  type select_marker = Take | Skip
  type t = ..
  type monitor = Process_down of Pid.t
  type t += Monitor of monitor
  type t += Exit of Pid.t
end

module Process : sig
  type t
end

type process_flag = Trap_exit of bool

type exit_reason =
  | Normal
  | Exit_signal
  | Timeout_value
  | Bad_link
  | Exception of exn

val yield : unit -> unit
(** Suspends execution of the current process and returns control to the scheduler *)

val self : unit -> Pid.t
(** Returns the process identifier (pid) for the current process *)

val process_flag : process_flag -> unit

val exit : Pid.t -> exit_reason -> unit
(** Sends an exit signal to the process [pid], to exit with reason [exit_reason] *)

val send : Pid.t -> Message.t -> unit
(** Sends a message to process with this pid. *)

val spawn : (unit -> unit) -> Pid.t
(** Spawns a new process. *)

val spawn_link : (unit -> unit) -> Pid.t
(** Spawns a new process and links it to the current process before returning. *)

exception Link_no_process of Pid.t

val link : Pid.t -> unit
(** Links the current process and the process [pid] together. *)

val monitor : Pid.t -> Pid.t -> unit
(** Makes [pid1] a monitor of [pid2]. When [pid2] terminates, [pid1] will receive a message. *)

val processes : unit -> (Pid.t * Process.t) Seq.t

val is_process_alive : Pid.t -> bool
(** Returns true if the process [pid] is still alive. *)

val random : unit -> Random.State.t
val receive : ?select:(Message.t -> Message.select_marker) -> unit -> Message.t

val shutdown : unit -> unit
(** Gracefully shuts down the runtime. Any non-yielding process will block this. *)

val run : ?rnd:Random.State.t -> ?workers:int -> (unit -> unit) -> unit
(** Start the Riot runtime using function [main] to boot the system *)

module Supervisor : sig
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

  type child_spec
  (* The type of a child specification *)

  val child_spec :
    start_link:('state -> (Pid.t, [> `Exit of exn ]) result) ->
    'state ->
    child_spec
  (** Create a new child specification to be used with [start_link] *)

  val start_link :
    ?strategy:strategy ->
    ?restart_limit:int ->
    ?restart_period:int ->
    child_specs:child_spec list ->
    unit ->
    (Pid.t, [> `Supervisor_error ]) result
  (** Describe and start a supervisor *)
end
