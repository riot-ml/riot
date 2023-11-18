open Runtime

type child_spec

val child_spec :
  start_link:('a -> (Pid.t, [> `Exit of exn ]) result) -> 'a -> child_spec

type strategy = One_for_one | One_for_all | Rest_for_one | Simple_one_for_one

val start_link :
  ?strategy:strategy ->
  ?restart_limit:int ->
  ?restart_period:int ->
  child_specs:child_spec list ->
  unit ->
  (Pid.t, 'a) result

val children : Pid.t -> Pid.t list
