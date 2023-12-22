# Changes

## Unreleased

* Redo packaging to expose a single public library: `riot`
* Fix issue with schedulers busy-waiting
* Introduce separate IO Schedulers to polling
* Switch to `poll` to support kqueue on macOS
* Reuse read-buffers on Io.read loops
* Add `Dashmap.iter` to iterate over a collection

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

