type _ Effect.t += Receive : { ref : unit Ref.t option } -> Message.t Effect.t
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

let pp : type a. Format.formatter -> a Effect.t -> unit =
 fun ppf eff ->
  match eff with
  | Receive _ -> Format.fprintf ppf "Receive"
  | Yield -> Format.fprintf ppf "Yield"
  | Syscall { name; fd; _ } -> Format.fprintf ppf "Syscall(%s,%a)" name Fd.pp fd
  | _effect -> Format.fprintf ppf "Unhandled effect"
