# `4-long-lived-processes`

Up until now, we have only dealt with processes that start, do some work, and
immediately die: either prints something and terminates, or waits for a message
and once the message arrives, it terminates.

But real systems are built out of long-living processes that can handle more
than a single message. How do we build those? Recursion, baby!

```ocaml
let pid = spawn (fun () -> loop ()) in
(* ... *)
```

To make a process that will live indefinitely, you just need to make a
recursive function. This has some advantages:

1. it is a very familiar way of programming in OCaml
2. it gives us State in a functional way (no mutation required!)

So let's do this! We'll write a process that recieves a message, says Hello to
someone, and continues awaiting.

```ocaml
let rec loop () =
  (match receive_any () with
  | Hello name -> print_endline ("Hello, " ^ name ^ "! :D")
  | _ -> print_endline "Oh no, an unhandled message! D:");
  loop ()
```

As we saw on the [message-passing tutorial](/3-message-passing/), processes can
receive all sorts of messages that we don't know about, although they will all
be typed, so we include a little catch-all to ignore unhandleable messages. 

One caveat is that because function application can't be interrupted, we need
to make sure we _yield_ control back to the scheduler at some point before
recursing. Otherwise one of the cores will be _blocked_ by this process until it yields.

In our example, this is done automatically when we call `receive_any`

In fact, we are strategically placing yields all through the standard library
to make it as seamless as possible to write Riot programs without thinking
about scheduler starvation.

## Next Steps

* the [next step](../5-links-and-monitors/) introduces you to links and
  monitors, to keep track of the lifecycle of a process or to make the
lifecycle of a process be linked to another
