open Util

type _ Effect.t +=
  | Receive : {
      ref : 'a Symbol.t option;
      timeout : Timeout.t;
    }
      -> Message.t Effect.t
  [@@unboxed]

type _ Effect.t += Yield : unit Effect.t [@@unboxed]

type _ Effect.t +=
  | Syscall : {
      name : string;
      mode : [ `r | `w | `rw ];
      fd : Fd.t;
    }
      -> unit Effect.t
  [@@unboxed]
