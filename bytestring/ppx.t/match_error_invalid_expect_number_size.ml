let _ = 
  match%bytestring {%bytestring| |} with
  | {| 2112::utf8, 0::1, rest::rush |} -> ()
