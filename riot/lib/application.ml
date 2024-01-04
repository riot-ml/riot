module type Intf = sig
  val name : string

  val start :
    unit ->
    ( Pid.t,
      ([> `Application_error of string | `Supervisor_error ] as 'err) )
    result
end
