type _ Effect.t +=
  | Receive : {
      select : Message.t -> Message.select_marker;
    }
      -> Message.t Effect.t
  | Yield : unit Effect.t

let pp : type a. Format.formatter -> a Effect.t -> unit =
 fun ppf eff ->
  match eff with
  | Receive _ -> Format.fprintf ppf "Receive"
  | Yield -> Format.fprintf ppf "Yield"
  | _effect -> Format.fprintf ppf "Unhandled effect"
