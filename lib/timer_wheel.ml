module Timer = struct
  type t = {
    id : unit Ref.t;
    mode : [ `interval | `one_off ];
    mutable started_at : Ptime.t;
    ends_at : Ptime.Span.t;
    fn : unit -> unit;
  }

  let pp fmt t =
    let mode = if t.mode = `interval then "interval" else "one_off" in
    Format.fprintf fmt "Timer { id=%a; started_at=%a; ends_at=%a; mode=%s }"
      Ref.pp t.id (Ptime.pp_rfc3339 ()) t.started_at Ptime.Span.pp t.ends_at
      mode

  let make time mode fn =
    let id = Ref.make () in
    let started_at = Ptime_clock.now () in
    let ends_at = Ptime.Span.of_float_s time |> Option.get in
    { id; started_at; ends_at; fn; mode }

  let equal a b = Ref.equal a.id b.id |> Option.is_some
end

type t = { timers : (Timer.t, unit) Dashmap.t; mutable last_t : Ptime.t }

let create () = { timers = Dashmap.create (); last_t = Ptime_clock.now () }

let make_timer t time mode fn =
  let timer = Timer.make time mode fn in
  Logs.trace (fun f -> f "Making timer: %a" Timer.pp timer);
  Dashmap.insert t.timers timer ();
  timer.id

let ends_at now t1 =
  let t0 = Ptime.to_span now in
  Ptime.Span.add t0 t1 |> Ptime.of_span |> Option.get

let run_timer t now (timer, ()) =
  let open Timer in
  let ends_at = ends_at timer.started_at timer.ends_at in
  Logs.trace (fun f ->
      f "Running timer %a with ends_at=%a" Timer.pp timer (Ptime.pp_rfc3339 ())
        ends_at);
  if Ptime.is_earlier now ~than:ends_at then ()
  else (
    (match timer.mode with
    | `one_off ->
        Dashmap.remove_by t.timers (fun (timer', ()) ->
            Timer.equal timer timer')
    | `interval -> timer.started_at <- now);
    timer.fn ())

let tick t =
  let now = Ptime_clock.now () in
  let timers = Dashmap.entries t.timers in
  List.iter (run_timer t now) timers;
  t.last_t <- now
