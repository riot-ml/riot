# Changes

## Unreleased

* Introduce SSL module to turn sockets into SSL-backed Reader/Writer streams.
  This includes making a `Net.Socket.stream_socket` into a client or a server
  SSL-backed stream pair.

* Introduce monotomic timers with nanosecond precision. Thansk to the `mtime`
  library this was a breeze!

* Introduce timeouts in `receive` – you can now call `receive ~after:10L ()`
  and if there are messages fetched in 10 microseconds `receive` will raise a
  `Receive_timeout` exception that you can match on.

* Introduce `Task` to quickly spin up processes that we can await. This is the
  closest we have to a future. A `Task` is typed, executes a single function, 
  and MUST be awaited with `Task.await ?timeout task`.

* Introduce specialized Dashmap's with the `Dashmap.Make` functor.

* Improve `Timer_wheel` with support for clearing timers, iterating timers
  in the order in which they were created, and a MinHeap backend.

## 0.0.7

* Introduce IO module with low-level IO operations such as performing direct
  vectorized (or regular) reads/writes. New operations include:
  * `read`, `write`
  * `single_read`, `single_write` (vectorized)
  * `await_readable`, `await_writeable`, `await`
  * `write_all`
  * `copy` and `copy_buffered`

* Introduce Buffer module with support for converting from and to CStruct and
  String, including position tracking.

* Introduce Read/Reader interface for creating buffered and unbuffered readers
  of arbitrary sources.

* Introduce Write/Writer interfaces for creating unbuffered writers into
  arbitrary destinations/sinks.

* Introduce File module with Reader and Writer implementations

* Implment Reader and Writer interfaces for Net.Socket

* Dropped dependency on Bigstringaf and moved to Cstruct

* Fix max number of domains to always be under the recommended domain count

* Fix issue with tests where the runtime idled after the main would die. Now
  the main process finishing with an exception is considered reason enough to
  shutdown the system.

* Refactor tests to always output `test_name: OK` when everything is fine and
  all modules to end in `_test`.

* Add several IO tests.

* Fix log levels for writing to sockets

* Include proper license for C Stubs copied from `lib_eio_posix` for vectorized i/o.

* Split test suite into io/non-io so io tests are left outside opam ci

* Improved IO polling that removes heavy iterations over process/fds tables

* Rewrite Dashmap internals to use a Hashtbl


## 0.0.6

* Redo packaging to expose a single public library: `riot`
* Fix issue with schedulers busy-waiting
* Introduce separate IO Schedulers for polling
* Switch to `poll` to support kqueue on macOS
* Reuse read-buffers on Io.read loops
* Broaden IO socket types to file descriptors
* Improved polling with shorter poll timeouts and safety checks
* Add `Dashmap.iter` to iterate over a collection
* Add `net_test` with an echo tcp server/client
* Fix bugs with syscall suspension that was introduced with reduction counting


## 0.0.5

* Add `register name pid`
* Add `unregister name`
* Add `send_by_name ~name msg`
* Fix timer wheel making it remove timers correctly
* Add better test for `Timer.send_after`

## 0.0.4

* Internally immediately suspend (bypassing reduction counts) when on a receive expression
* Fix reads from closed Unix sockets
* Fix writes to closed Unix sockets
* Ignore SIGPIPEs on setup
* Fix always mark connected sockets as nonblocking 
* Fix GC i/o process table
* Surface pretty-printing of socket values 

## 0.0.3

* Big namespace refactor. `Riot.Runtime` includes the lower-level runtime
  blocks, and everything else that is more user-friendly lives at the `Riot.*`
level.
* Introduce reduction counting, so processes will run up to N iterations unless
  they finish, or they execute an unhandled effect.
* Introduce the `Application` interface for managing the lifecycle of the system
* Fix `Riot.Logger` to fit the `Application` interface
* Add a new `Riot.Telemetry` backend for doing async telemetry

## 0.0.2

* New `Riot.random ()` API to expose current scheduler's random state
* Better logging in the `Net` module
* Fix a bug where `Net.Socket` operations where hanging on I/O polling when they could have been eager

## 0.0.1

First release, including:

* First working version of the scheduler
* Support for process spawning, message passing, monitoring, and linking
* Rudimentary supervisors
* Basic (and incomplete) GenServer
* Scheduling-aware I/O primitives
* Scheduling-aware Logger
* Timers

