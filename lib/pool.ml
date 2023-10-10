type t = {
  mutable stop : bool;
  schedulers : Scheduler.t list;
  processes : Proc_table.t;
}
