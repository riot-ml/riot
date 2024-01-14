module Impl = Kqueue [@@config any (target_os = "linux", target_os = "android")]

module Impl = Kqueue
[@@config
  any
    ( target_os = "macos",
      target_os = "ios",
      target_os = "tvos",
      target_os = "watchos",
      target_os = "freebsd",
      target_os = "netbsd" )]

include Impl
