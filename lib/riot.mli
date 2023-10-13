module Ref : sig
  type 'a t

  val make : unit -> 'a t
  val pp : Format.formatter -> 'a t -> unit
  val equal : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option
end

module Pid : sig
  type t
  (** A process identifier. Use values of this type to check if processes are
      still alive, to send them messages, to link to them, to monitor them, and
      to send them exit signals. *)

  val zero : t
  (** [zero] is the empty pid. It represents the first process created by the
      runtime. This is typically the `main` function passed to `Riot.run` *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Message : sig
  (** [select_marker] is used in a _selective receive_ call to match the exact
      messages you are looking for. This is useful to skip ahead in your
      mailbox without having to consume all the messages in it.
  *)
  type select_marker =
    | Take  (** use [Take] to mark a message as selected *)
    | Skip  (** use [Skip] to requeue for later consumption *)
    | Drop  (** use [Drop] to remove this message while selecting *)

  type t = ..
  (** [t] is the type of all messages in a Riot program.

      Since this type is extensible, you can make sure different parts of your
      program can see the constructors that are relevant for them.
    *)
end

module Process : sig
  type t
  (** [t] is the type of all processes in the Riot runtime.

      A process is a lightweight unit of work that has a lifecycle and a mailbox.

      You rarely work directly with this type, and usually handle {!type:Pid.t}
      values instead.
   *)

  val pp : Format.formatter -> t -> unit
  val get_pid : t -> Pid.t

  (** A process flag is a configuration for the behavior of a process. *)
  type process_flag =
    | Trap_exit of bool
        (** [Trap_exit true] makes sure this process does not exit when it
            receives an Exit message (see {!module:Process.Messages}) from a
            linked process that has died. *)

  (* An [exit_reason] describes why a process finished. *)
  type exit_reason =
    | Normal  (** The process ended normally. *)
    | Exit_signal  (** The process received an exit signal *)
    | Bad_link  (** The process tried to establish a bad link *)
    | Link_down of Pid.t
        (** Use to indicate that this process was terminated due to a linked process being termianted *)
    | Exception of exn
        (** The process terminated due to an unhandled exception *)

  module Messages : sig
    type monitor =
      | Process_down of Pid.t
          (** This monitor message reports that a monitored process has terminated. *)

    type Message.t +=
      | Monitor of monitor  (** Monitor related messages *)
      | Exit of Pid.t * exit_reason
            (** Exit signal. If you want this message make sure to set the
                [Trap_exit] flag to true with the `process_flag` function. *)
  end
end

val yield : unit -> unit
(** Suspends execution of the current process and returns control to the scheduler *)

val sleep : float -> unit
(** [sleep t] Suspends execution of the current process for at least `t` seconds.
    `t` is a float so it supports subsecond values: `0.001` is 1 millisecond.
  *)

val self : unit -> Pid.t
(** Returns the process identifier (pid) for the current process *)

val process_flag : Process.process_flag -> unit

val exit : Pid.t -> Process.exit_reason -> unit
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

val wait_pids : Pid.t list -> unit
(** Await all processes in the list to termimante. *)

val random : unit -> Random.State.t

val receive : ?select:(Message.t -> Message.select_marker) -> unit -> Message.t
(** [receive select ()] will block the current process until a message that
    matches the [select] function is found in the mailbox, and then will pop it
    and return it.

    This function will suspend a process that has an empty mailbox, and the
    process will remain asleep until a message is delivered to it.
*)

val shutdown : unit -> unit
(** Gracefully shuts down the runtime. Any non-yielding process will block this. *)

val run : ?rnd:Random.State.t -> ?workers:int -> (unit -> unit) -> unit
(** Start the Riot runtime using function [main] to boot the system *)

val get_proc : Pid.t -> Process.t
val trace_send : (Pid.t -> Process.t -> Message.t -> unit) -> unit
val trace_proc_run : (int -> Process.t -> unit) -> unit

(* Generic Servers *)

module Gen_server : sig
  type 'res req = ..
  (** [req] is the type of all generic server requests and responses.

      When defining a new generic server you want to extend this with the your
      custom request types, including the response type in its type variable.
      Like this: 

      ```ocaml
      open Riot
      type _ Gen_server.req +=
        | Is_connected : bool Gen_server.req
        | Profile : profile_req -> profile_res Gen_server.req
      ```
    *)

  (** [state init_result] is used to initialize a new generic server. *)
  type 'state init_result =
    | Ok of 'state
        (** use this value to enter the main loop with state ['state] *)
    | Error
        (** use this value to crash the process and notify a supervisor of it *)
    | Ignore  (** use this value to exit the process normally *)

  (** [Impl] is the module type of the generic server base implementations. You
      can use this type when defining new gen servers like this:

      ```ocaml
      type args = int
      module Server : Gen_server.Impl with type args = args = struct
        type nonrec args = args
        type state = { status : int }

        let init _args = Gen_server.Ok { status = 1 }

        (* ... *)
      end
      ```
  *)
  module type Impl = sig
    type args
    type state

    val init : args -> state init_result
    val handle_call : 'res. 'res req -> Pid.t -> state -> 'res
  end

  type ('args, 'state) impl =
    (module Impl with type args = 'args and type state = 'state)

  val call : Pid.t -> 'res req -> 'res
  (** [call pid req] will send a type-safe request [req] to the generic server behind [pid]
      that is guaranteed to return a respone with type `'res`

      This function will block the current process until a response arrives.

      TODO(leostera): add ?timeout param
    *)

  val start_link : ('args, 'state) impl -> 'args -> (Pid.t, exn) result
  (** [start_link (module S) args] will spawn and link a new process that will
      act as a generic server over the server implementation of [S],
      initialized with [args] arguments.
  *)
end

(* Supervisor *)

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

(* Logger *)

type ('a, 'b) logger_format =
  (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

module type Logger = sig
  val set_log_level : Logger.level option -> unit
  val debug : ('a, unit) logger_format -> unit
  val error : ('a, unit) logger_format -> unit
  val info : ('a, unit) logger_format -> unit
  val trace : ('a, unit) logger_format -> unit
  val warn : ('a, unit) logger_format -> unit
end

module Logger : sig
  type namespace = string list

  val start :
    ?print_time:bool ->
    ?print_source:bool ->
    ?color_output:bool ->
    unit ->
    (unit, [> `Supervisor_error ]) result
  (** Start the logger application. *)

  module type Namespace = sig
    val namespace : namespace
  end

  module Make (_ : Namespace) : Logger
  include Logger.Intf
end
