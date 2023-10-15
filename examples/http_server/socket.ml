[@@@warning "-37"]
[@@@warning "-32"]
[@@@warning "-69"]
[@@@warning "-8"]

open Riot

module Logger = Logger.Make (struct
  let namespace = [ "socket" ]
end)

let trace, info, debug, warn, error = Logger.(trace, info, debug, warn, error)
