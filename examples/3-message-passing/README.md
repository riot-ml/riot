# `3-message-passing`

Now that we've learned to spawn processes, we can start sending messages to
them.

Every message in Riot is _typed_. And all messages form part of the `Message.t`
type. To define a new message, you can write:

```ocaml
type Message.t += Hello_world
```

Your message can have any shape you want, so long as it fits into this message
type. Once a message is defined, we can start a process that knows how to
receive them. To receive a message we use the `receive` function, like this:

```ocaml
match receive () with
| Hello_world -> print_endline "Hello, World! :D"
```

`receive ()` will try to get a message from the _current process mailbox_. If
the mailbox is empty, `receive ()` _will suspend the process_ until a message
is delivered.

Since messages are represented with an open variant, when we pattern match on
`receive ()` we will have to make sure to handle or ignore _other messages_.

```ocaml
match receive () with
| Hello_world -> print_endline "Hello, World! :D"
| _ -> print_endline "Oh no, an unhandled message! D:"
```

Within a process, it is okay for us to do a _partial match_, since a process crashing isn't going to take the runtime down. So an alternative way to write this is:

```ocaml
match[@warning "-8"] receive () with
| Hello_world -> print_endline "Hello, World! :D"
```

Although I would recommend you to be careful where you disable this warning,
since exhaustive pattern-matching is one of OCaml's best features.

## Next Steps

* the [next step](../4-long-lived-processes/) introduces you to long lived processes
