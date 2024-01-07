(** {1 Riot}

*)

module Timeout : sig
  type t = [ `infinity | `after of int64 ]
end

module Symbol : sig
  type 'a t
  (** A unique reference.

      A value of `'a t` won't be created twice (but can be shared/copied),
      which makes it ideal for coordination between processes.

      Normally, you'd use a `'a Symbol.t` to identify outgoing/incoming message
      pairs, but they can also be used for type-equalities. If two refs of type
      `'a Symbol.t` and `'b Symbol.t` are equal, then you can use `Symbol.type_equal`
      to obtain a type-level witness that proves that `'a` and `'b` are equal.
  *)

  val make : unit -> 'a t
  (** `make ()` creates a new unique ref. The type of the ref may be inferred
      from context or explicitly set.
  *)

  val pp : Format.formatter -> 'a t -> unit

  val equal : 'a t -> 'b t -> bool
  (** `equal ref1 ref2` returns true if both references are the same,
      regardless of the type they hold.

      If the types are different, you may want to use `type_equal` instead to
      prove type equality.
  *)

  val type_equal : 'a 'b. 'a t -> 'b t -> ('a, 'b) Type.eq option
  (** `type_equal refA refB` proves that `'a` and `'b` are equals if the
      underlying refs are also equal.
  *)
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
  (** `equal pid1 pid2` returns true if both pids are the same. *)

  val pp : Format.formatter -> t -> unit
end

module Message : sig
  type t = ..
  (** [t] is the type of all messages in a Riot program.

      Since this type is extensible, you can make sure different parts of your
      program can see the constructors that are relevant for them.
    *)

  (** [select_marker] is used in a _selective receive_ call to match the exact
      messages you are looking for. This is useful to skip ahead in your
      mailbox without having to consume all the messages in it.
  *)
  type select_marker =
    | Take  (** use [Take] to mark a message as selected *)
    | Skip  (** use [Skip] to requeue for later consumption *)
    | Drop  (** use [Drop] to remove this message while selecting *)
end

module Process : sig
  type t
  (** [t] is the type of all processes in the Riot runtime.

      A process is a lightweight unit of work that has a lifecycle and a mailbox.

      You rarely work directly with this type, and usually handle {!type:Pid.t}
      values instead.
   *)

  val pp : Format.formatter -> t -> unit

  (** The priority for a process.

      A processes' priority can be adjusted to guarantee it always runs before
      other processes, or to indicate that it is less important that a process
      executes timely as long as it executes at some point in the future.

      * High priority processes are scheduled and executed before everything
        else. Use this with care or your other priorities may have to wait a
        long time before they run.

      * Normal priority processes are executed when there are no priority
        processes ready to be executed.

      * Low priority processes are only executed when there are no High and no
        Normal priority processes left.

   *)
  type priority = High | Normal | Low

  (** A process flag is a configuration for the behavior of a process. *)
  type process_flag =
    | Trap_exit of bool
        (** [Trap_exit true] makes sure this process does not exit when it
            receives an Exit message (see {!module:Process.Messages}) from a
            linked process that has died. *)
    | Priority of priority
        (** Processes with a [High] priority will be scheduled before processes
           with a [Normal] priority which will be scheduled before processes
           with a [Low] priority. *)

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

  val where_is : string -> Pid.t option
  (** [where_is name] returns the {Pid.t} that is registered to [name] or
      [None] if no process was registered for that name.
  *)

  val await_name : string -> Pid.t
  (** [await_name name] waits until [name] is registered to a pid.

      NOTE: this function will block the process indefinitely.
  *)

  val demonitor : Pid.t -> unit
  (* [demonitor pid] removes the monitor from ourselves to [pid].

     This means that when [pid] dies, we will not receive a message.

     If we call [demonitor pid] {i after} [pid] died and the message was queued,
     we will receive the monitoring message.
  *)
end

(** A Riot `Application` can be used to encapsulate functionality that must
    share the same lifecycle. For example, the `Riot.Logger` is an Application.

    Applications are also useful to orchestrate the order of startup, since
    `Riot.start ~apps` will start them one by one.
*)
module Application : sig
  module type Intf = sig
    val start :
      unit ->
      ( Pid.t,
        ([> `Application_error of string | `Supervisor_error ] as 'err) )
      result
  end
end

val random : unit -> Random.State.t
(** Returns the current random state from a scheduler. *)

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

exception Invalid_destination of string

val send_by_name : name:string -> Message.t -> unit
(** Sends a message to a process registered with [name]. If [name] is not a
    valid destination for a message, this function raises an
    [Invalid_destination name] exception. *)

val spawn : (unit -> unit) -> Pid.t
(** Spawns a new process. *)

val spawn_link : (unit -> unit) -> Pid.t
(** Spawns a new process and links it to the current process before returning. *)

exception Name_already_registered of string * Pid.t

val register : string -> Pid.t -> unit
(** [register name pid] registers a process by a given name. The name will be
    uniquely associated to this process and attempting to register the same
    name twice will result in an exception [Name_already_registered] being
    raised. *)

val unregister : string -> unit
(** [unregister name] frees a name and allows it to be re-registered. If the
    name was not registered before, this operation does nothing. *)

exception Link_no_process of Pid.t

val link : Pid.t -> unit
(** Links the current process and the process [pid] together. *)

val monitor : Pid.t -> unit
(** [monitor pid] makes [self ()] a monitor of [pid].

    When [pid] terminates, [self ()] will receive a
    [Processes.Messages.Monitor(Process_down(pid))] message.
*)

val processes : unit -> (Pid.t * Process.t) Seq.t
(** `processes ()` will list all the processes currently alive. *)

val is_process_alive : Pid.t -> bool
(** Returns true if the process [pid] is still alive. *)

val wait_pids : Pid.t list -> unit
(** Await all processes in the list to termimante. *)

exception Receive_timeout

val receive : ?after:int64 -> ?ref:unit Symbol.t -> unit -> Message.t
(** [receive ()] will return the first message in the process mailbox.

    This function will suspend a process that has an empty mailbox, and the
    process will remain asleep until a message is delivered to it.

    ### Timed Receive

    If a `after was passed, then `[receive ~after ()]` will wait up to [after]
    and raise an `Receive_timeout` exception that can be matched on.

    This is useful to prevent deadlock of processes when receiving messages.

    ### Selective Receive

    If a `ref` was passed, then `[receive ~ref ()]` will skip all messages
    created before the creation of this `Symbol.t` value, and will only return
    newer messages.

    This is useful to skip the queue, but not remove any of the messages before
    it. Those messages will be delivered in-order in future calls to `receive
    ()`.
*)

val shutdown : unit -> unit
(** Gracefully shuts down the runtime. Any non-yielding process will block this. *)

val run : ?rnd:Random.State.t -> ?workers:int -> (unit -> unit) -> unit
(** Start the Riot runtime using function [main] to boot the system *)

val start :
  ?rnd:Random.State.t ->
  ?workers:int ->
  apps:(module Application.Intf) list ->
  unit ->
  unit
(** Start the Riot runtime with a series of applications.

    Each application will be started in the same order as specified, and
    if any application fails to start up, the system will be shutdown.

    Once the applications are started, they will all be monitored until they
    are all terminated. Only then will the runtime shutdown.
*)

val trace_send : (Pid.t -> Process.t -> Message.t -> unit) -> unit
val trace_proc_run : (int -> Process.t -> unit) -> unit

(* Generic Servers *)

module Gen_server : sig
  type 'res req = ..
  (** [req] is the type of all generic server requests and responses.

      When defining a new generic server you want to extend this with the your
      custom request types, including the response type in its type variable.
      Like this:

      {@ocaml[
      open Riot
      type _ Gen_server.req +=
        | Is_connected : bool Gen_server.req
        | Profile : profile_req -> profile_res Gen_server.req
      ]}
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

      {@ocaml[
      type args = int
      module Server : Gen_server.Impl with type args = args = struct
        type nonrec args = args
        type state = { status : int }

        let init _args = Gen_server.Ok { status = 1 }

        (* ... *)
      end
      ]}
  *)
  module type Impl = sig
    type args
    type state

    val init : args -> state init_result
    val handle_call : 'res. 'res req -> Pid.t -> state -> 'res
    val handle_info : Message.t -> state -> unit
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
    ('state -> (Pid.t, [> `Exit of exn ]) result) -> 'state -> child_spec
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

(* Telemetry *)

module Telemetry : sig
  include Application.Intf

  type event = Telemetry.event = ..

  val emit : event -> unit
  val attach : (event -> unit) -> unit
end

(* Logger *)

module Logger : sig
  type level = Debug | Error | Info | Trace | Warn

  type ('a, 'b) logger_format =
    (('a, Format.formatter, unit, 'b) format4 -> 'a) -> 'b

  module type Intf = sig
    val set_log_level : level option -> unit
    val debug : ('a, unit) logger_format -> unit
    val error : ('a, unit) logger_format -> unit
    val info : ('a, unit) logger_format -> unit
    val trace : ('a, unit) logger_format -> unit
    val warn : ('a, unit) logger_format -> unit
  end

  include Application.Intf

  type opts = { print_source : bool; print_time : bool; color_output : bool }
  type namespace = string list

  module type Namespace = sig
    val namespace : namespace
  end

  module Make (_ : Namespace) : Intf
  include Intf
end

module Fd : sig
  module Mode : sig
    type t = [ `r | `rw | `w ]

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  type fd = Unix.file_descr
  type t

  exception Already_closed of string

  val get : t -> fd option
  val to_int : t -> int
  val make : fd -> t
  val is_open : t -> bool
  val is_closed : t -> bool
  val close : t -> unit
  val use : op_name:string -> t -> (fd -> 'a) -> 'a
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module IO : sig
  type unix_error = [ `Timeout | `Process_down | `Unix_error of Unix.error ]
  type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

  val pp_err :
    Format.formatter ->
    [< unix_error | `Closed | `Timeout | `Process_down | `System_limit ] ->
    unit

  module Buffer : sig
    type t

    val empty : t
    val concat : t -> t -> t
    val split : ?max:int -> on:string -> t -> t list
    val as_cstruct : t -> Cstruct.t
    val consume : t -> int -> unit
    val copy : src:t -> dst:t -> int
    val discard : t -> unit
    val filled : t -> int
    val is_empty : t -> bool
    val is_full : t -> bool
    val length : t -> int
    val of_cstruct : ?filled:int -> Cstruct.t -> t
    val of_string : string -> t
    val position : t -> int
    val set_filled : t -> filled:int -> unit
    val sub : ?off:int -> ?len:int -> t -> t
    val to_string : t -> string
    val with_capacity : int -> t
  end

  type read = [ `Abort of Unix.error | `Read of int | `Retry ]
  type write = [ `Abort of Unix.error | `Retry | `Wrote of int ]

  val read : Fd.t -> bytes -> int -> int -> read
  val write : Fd.t -> bytes -> int -> int -> write
  val await_readable : Fd.t -> (Fd.t -> 'a) -> 'a
  val await_writeable : Fd.t -> (Fd.t -> 'a) -> 'a
  val await : Fd.t -> Fd.Mode.t -> (Fd.t -> 'a) -> 'a
  val single_read : Fd.t -> buf:Buffer.t -> (int, [> `Closed ]) result
  val single_write : Fd.t -> data:Buffer.t -> (int, [> `Closed ]) result

  module type Write = sig
    type t

    val write : t -> data:Buffer.t -> (int, [> `Closed ]) result
    val flush : t -> (unit, [> `Closed ]) result
  end

  module Writer : sig
    type 'src t
    type 'src write = (module Write with type t = 'src)

    val of_write_src : 'src. 'src write -> 'src -> 'src t
    val write : 'src t -> data:Buffer.t -> (int, [> `Closed ]) result

    module Make (B : Write) : sig
      type t = B.t

      val write : t -> data:Buffer.t -> (int, [> `Closed ]) result
      val flush : t -> (unit, [> `Closed ]) result
    end
  end

  module type Read = sig
    type t

    val read : t -> buf:Buffer.t -> (int, [> `Closed ]) result
  end

  module Reader : sig
    type 'src t
    type 'src reader = 'src t
    type 'src read = (module Read with type t = 'src)

    val of_read_src : 'src. 'src read -> 'src -> 'src t
    val read : 'src reader -> buf:Buffer.t -> (int, [> `Closed ]) result

    module Make (B : Read) : sig
      type t = B.t

      val read : t -> buf:Buffer.t -> (int, [> `Closed ]) result
    end

    module Buffered : sig
      type 'src t

      val of_reader : ?capacity:int -> 'src reader -> 'src t reader
      val to_buffer : 'src t -> Buffer.t
    end

    val of_buffer : Buffer.t -> unit Buffered.t t
  end

  val write_all : 'dst Writer.t -> data:Buffer.t -> (int, [> `Closed ]) result

  val copy_buffered :
    'src Reader.Buffered.t Reader.t ->
    'dst Writer.t ->
    (int, [> `Closed ]) result

  val copy :
    ?buf:Buffer.t ->
    'src Reader.t ->
    'dst Writer.t ->
    (int, [> `Closed ]) result
end

module Net : sig
  module Addr : sig
    type tcp_addr
    type stream_addr

    val to_string : tcp_addr -> string
    val ip : stream_addr -> tcp_addr
    val port : stream_addr -> int
    val get_info : stream_addr -> stream_addr list
    val loopback : tcp_addr
    val of_unix : Unix.sockaddr -> stream_addr
    val of_uri : Uri.t -> stream_addr option
    val pp : Format.formatter -> stream_addr -> unit
    val tcp : tcp_addr -> int -> stream_addr
    val to_domain : stream_addr -> Unix.socket_domain
    val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
  end

  module Socket : sig
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket

    type listen_opts = {
      reuse_addr : bool;
      reuse_port : bool;
      backlog : int;
      addr : Addr.tcp_addr;
    }

    val listen :
      ?opts:listen_opts ->
      port:int ->
      unit ->
      (listen_socket, [> `System_limit ]) IO.result

    val connect : Addr.stream_addr -> (stream_socket, [> `Closed ]) IO.result

    val accept :
      ?timeout:int64 ->
      listen_socket ->
      (stream_socket * Addr.stream_addr, [> `Closed | `Timeout ]) IO.result

    val close : _ socket -> unit

    val controlling_process :
      _ socket -> new_owner:Pid.t -> (unit, [> `Closed | `Not_owner ]) IO.result

    val receive :
      ?timeout:int64 ->
      buf:IO.Buffer.t ->
      stream_socket ->
      (int, [> `Closed | `Timeout ]) IO.result

    val send :
      ?timeout:int64 ->
      data:IO.Buffer.t ->
      stream_socket ->
      (int, [> `Closed ]) IO.result

    val pp : Format.formatter -> _ socket -> unit

    val pp_err :
      Format.formatter ->
      [ IO.unix_error | `Closed | `Timeout | `Process_down | `System_limit ] ->
      unit

    val to_reader : ?timeout:int64 -> stream_socket -> stream_socket IO.Reader.t
    val to_writer : ?timeout:int64 -> stream_socket -> stream_socket IO.Writer.t
  end
end

module File : sig
  type 'kind file

  val fd : _ file -> Fd.t
  val open_read : string -> [ `r ] file
  val open_write : string -> [ `w ] file
  val close : _ file -> unit
  val remove : string -> unit
  val to_reader : [ `r ] file -> [ `r ] file IO.Reader.t
  val to_writer : [ `w ] file -> [ `w ] file IO.Writer.t
  val stat : string -> Unix.stats

  val send :
    ?off:int ->
    len:int ->
    [ `r ] file ->
    Net.Socket.stream_socket ->
    (int, [> `Closed ]) IO.result
end

module SSL : sig
  type 'src t

  exception Tls_alert of Tls.Packet.alert_type
  exception Tls_failure of Tls.Engine.failure

  val of_server_socket :
    ?read_timeout:int64 ->
    ?send_timeout:int64 ->
    ?config:Tls.Config.server ->
    Net.Socket.stream_socket ->
    Net.Socket.stream_socket t

  val of_client_socket :
    ?read_timeout:int64 ->
    ?send_timeout:int64 ->
    ?host:[ `host ] Domain_name.t ->
    config:Tls.Config.client ->
    Net.Socket.stream_socket ->
    Net.Socket.stream_socket t

  val to_reader : 'src t -> 'src t IO.Reader.t
  val to_writer : 'dst t -> 'dst t IO.Writer.t

  val negotiated_protocol :
    'src t ->
    (string option, [> `Inactive_tls_engine | `No_session_data ]) result
end

module Timer : sig
  type timer

  val send_after :
    Pid.t -> Message.t -> after:int64 -> (timer, [> `Timer_error ]) result

  val send_interval :
    Pid.t -> Message.t -> every:int64 -> (timer, [> `Timer_error ]) result

  val cancel : timer -> unit
end

module Queue : sig
  exception Closed

  type 'a t

  val push : 'a t -> 'a -> unit
  val push_head : 'a t -> 'a -> unit
  val close : 'a t -> unit
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a option
  val is_empty : 'a t -> bool
  val create : unit -> 'a t
end

(** Riot's [Hashmap] is a process-safe implementation of a hash-table. *)
module Hashmap : sig
  type ('k, 'v) t

  val create : ?size:int -> unit -> ('k, 'v) t
  val get : ('k, 'v) t -> 'k -> 'v option
  val get_all : ('k, 'v) t -> 'k -> 'v list
  val is_empty : ('k, 'v) t -> bool
  val find_by : ('k, 'v) t -> ('k * 'v -> bool) -> ('k * 'v) option
  val remove : ('k, 'v) t -> 'k -> unit
  val remove_all : ('k, 'v) t -> 'k list -> unit
  val find_all_by : ('k, 'v) t -> ('k * 'v -> bool) -> ('k * 'v) list
  val has_key : ('k, 'v) t -> 'k -> bool
  val insert : ('k, 'v) t -> 'k -> 'v -> unit
  val remove_by : ('k, 'v) t -> ('k * 'v -> bool) -> unit
  val replace : ('k, 'v) t -> 'k -> 'v -> unit
  val iter : ('k, 'v) t -> ('k * 'v -> unit) -> unit

  val pp :
    (Format.formatter -> 'k -> unit) -> Format.formatter -> ('k, 'v) t -> unit

  module type Base = sig
    type key

    val hash : key -> int
    val equal : key -> key -> bool
  end

  module type Intf = sig
    type key
    type 'v t

    val create : ?size:int -> unit -> 'v t
    val keys : 'v t -> key Seq.t
    val get : 'v t -> key -> 'v option
    val get_all : 'v t -> key -> 'v list
    val is_empty : 'v t -> bool
    val find_by : 'v t -> (key * 'v -> bool) -> (key * 'v) option
    val remove : 'v t -> key -> unit
    val remove_all : 'v t -> key list -> unit
    val find_all_by : 'v t -> (key * 'v -> bool) -> (key * 'v) list
    val has_key : 'v t -> key -> bool
    val insert : 'v t -> key -> 'v -> unit
    val remove_by : 'v t -> (key * 'v -> bool) -> unit
    val replace : 'v t -> key -> 'v -> unit
    val iter : 'v t -> (key * 'v -> unit) -> unit

    val pp :
      (Format.formatter -> key -> unit) -> Format.formatter -> 'v t -> unit
  end

  module Make (B : Base) : Intf with type key = B.key
end

module Task : sig
  type 'a t

  val async : (unit -> 'a) -> 'a t

  val await :
    ?timeout:int64 -> 'a t -> ('a, [> `Process_down | `Timeout ]) result
end

module Runtime : sig
  val set_log_level : Logger.level option -> unit
  val syscalls : unit -> int * int * int * int
end
