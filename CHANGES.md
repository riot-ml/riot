# Changes

## Unreleased

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

