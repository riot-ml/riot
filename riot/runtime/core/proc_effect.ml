open Gluon
open Util

type _ Effect.t +=
  | Receive : {
      ref : 'a Ref.t option;
      timeout : Timeout.t;
    }
      -> Message.t Effect.t
  [@@unboxed]

type _ Effect.t += Yield : unit Effect.t [@@unboxed]

type _ Effect.t +=
  | Syscall : {
      name : string;
      interest : Interest.t;
      source : Source.t;
      timeout : Timeout.t;
    }
      -> unit Effect.t
  [@@unboxed]
