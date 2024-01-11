let _ = 
  match%b {%b| |} with
  | {| 2112::utf8, 0::1, rest::rush |} -> ()
