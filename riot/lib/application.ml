module type Intf = sig
  val start :
    unit ->
    ( Pid.t,
      ([> `Application_error of string | `Supervisor_error ] as 'err) )
    result
end
