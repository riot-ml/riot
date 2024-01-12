(**
   The Timer Wheel keeps track of all the timers per scheduler, and is in
   charge of triggering them and cleaning them up.

   It is structured as 2 lists of timers, and a lookup table of timers per
   [tid] (timer id) that can be used to mark a timer as cancelled before it
   triggers.

   The list of timers are used for appending timers and for traversing them,
   where [timers] is the left-to-right traversal of timers in the order they
   were created, and [next_timers] will reversed and appended to [timers]
   before every iteration, but allows creating a timer in constant time.

   On every iteration, the timer wheel will go through the list of timers:

   * When a timer is [`finished], it will be removed off the list.

   * When a timer is triggered, its associated function will be executed. If
     the timer mode is [`interval], then the timer will be updated to trigger
     again in the expected time. If the timer mode is [`one_off]) then it will
     be removed from the list.

   * Otheriwse, a timer with [`pending] status will be kept in the list for the
     next iteration of the timer wheel.

*)

open Core
open Util

(** A Timer in the Riot runtime. *)
module Timer = struct
  type t = {
    id : unit Ref.t;
    mode : [ `interval | `one_off ];
    mutable status : [ `pending | `finished ];
    mutable started_at : Mtime.t;
    mutable timeouts_at : Mtime.t;
    ends_at : Mtime.Span.t;
    fn : unit -> unit;
  }

  let pp fmt t =
    let mode = if t.mode = `interval then "interval" else "one_off" in
    Format.fprintf fmt "Timer { id=%a; started_at=%a; ends_at=%a; mode=%s }"
      Ref.pp t.id Mtime.pp t.started_at Mtime.Span.pp t.ends_at mode

  let make time mode fn =
    let id = Ref.make () in
    let started_at = Mtime_clock.now () in
    let ends_at = Mtime.Span.of_uint64_ns Int64.(mul 1_000L time) in
    let timeouts_at = Mtime.add_span started_at ends_at |> Option.get in
    { id; started_at; ends_at; timeouts_at; fn; mode; status = `pending }

  let equal a b = Ref.equal a.id b.id
  let is_finished t = t.status = `finished

  let mark_as_cancelled t =
    Log.debug (fun f -> f "Cancelled timer %a" pp t);
    t.status <- `finished

  let leq a b = Mtime.compare a.timeouts_at b.timeouts_at <= 0
end

module TimeHeap = Min_heap.Make (Timer)

type t = {
  mutable timers : TimeHeap.t;
  mutable timer_count : int;
  ids : Timer.t Ref.Map.t;
  mutable last_t : Mtime.t; [@warning "-69"]
}

let create () =
  {
    timers = TimeHeap.empty;
    ids = Ref.Map.create ();
    last_t = Mtime_clock.now ();
    timer_count = 0;
  }

let can_tick t = t.timer_count > 0
let size t = t.timer_count

let is_finished t tid =
  match Ref.Map.get t.ids tid with
  | None -> true
  | Some timer -> Timer.is_finished timer

let remove_timer t timer =
  let timers = Ref.Map.get_all t.ids timer in
  List.iter Timer.mark_as_cancelled timers;
  t.timer_count <- t.timer_count - 1;
  Ref.Map.remove_by t.ids (fun (k, _) -> Ref.equal k timer)

let clear_timer t tid =
  let timer = Ref.Map.get t.ids tid in
  Option.iter Timer.mark_as_cancelled timer;
  t.timer_count <- t.timer_count - 1;
  Ref.Map.remove t.ids tid

let make_timer t time mode fn =
  let timer = Timer.make time mode fn in
  t.timers <- TimeHeap.insert timer t.timers;
  t.timer_count <- t.timer_count + 1;
  Ref.Map.insert t.ids timer.id timer;
  Log.debug (fun f -> f "Created timer %a" Timer.pp timer);
  timer.id

let run_timer now timer =
  let open Timer in
  if Timer.is_finished timer then (
    Log.debug (fun f -> f "Removing already finished timer: %a" Timer.pp timer);
    None)
  else
    let ends_at = Mtime.add_span timer.started_at timer.ends_at |> Option.get in
    let timeout = Mtime.is_later now ~than:ends_at || Mtime.equal now ends_at in
    Log.debug (fun f ->
        f "now(%a) > ends_at(%a) = %b" Mtime.pp now Mtime.pp ends_at timeout);
    Log.debug (fun f ->
        f "Running timer %a with ends_at=%a -> timeout? %b" Timer.pp timer
          Mtime.pp ends_at timeout);
    if timeout then (
      Log.debug (fun f -> f "timer timedout!");
      timer.fn ();
      match timer.mode with
      | `one_off ->
          Log.debug (fun f -> f "Removing timer");
          Timer.mark_as_cancelled timer;
          None
      | `interval ->
          Log.debug (fun f -> f "Requeuing timer");
          timer.started_at <- now;
          timer.timeouts_at <-
            Mtime.add_span timer.started_at timer.ends_at |> Option.get;
          Some timer)
    else (
      Log.debug (fun f -> f "no timeout yet, continuing");
      Some timer)

let rec run_timers t now timers =
  match TimeHeap.take timers with
  | None -> TimeHeap.empty
  | Some (_, timer) when Mtime.is_later timer.timeouts_at ~than:now -> timers
  | Some (timers', timer) -> (
      match run_timer now timer with
      | None ->
          t.timer_count <- t.timer_count - 1;
          run_timers t now timers'
      | Some timer ->
          let timers'' = TimeHeap.insert timer timers' in
          run_timers t now timers'')

let tick t =
  let now = Mtime_clock.now () in
  Log.trace (fun f -> f "Started Ticking timers %a" Mtime.pp now);
  t.timers <- run_timers t now t.timers;
  t.last_t <- now;
  Log.trace (fun f -> f "Done Ticking timers %a" Mtime.pp (Mtime_clock.now ()))
