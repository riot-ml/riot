# Sleep Sets

we have a scheduler
it has a queue of processes to run (these are just pointers to heap structs)


on every loop we pull a process from the run_queue
  we step over it
  if it ends up in waiting state
    we push it to the wait_queue
  if it ends up in runnable state
    we push it back into the run_queue

if we have no more processes in the run_queue
and our wait_queue is empty
and we are not shutting down
  then we become idle and wait to be awaken

on every loop we go over the wait_queue
  if a process should be awaken
    we push it to the run_queue
  else
    we keep it in the wait_queue
