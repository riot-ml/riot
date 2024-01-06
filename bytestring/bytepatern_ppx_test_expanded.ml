[@@@ocaml.ppx.context
{
  tool_name = "ppx_driver";
  include_dirs = [];
  load_path = [];
  open_modules = [];
  for_package = None;
  debug = false;
  use_threads = false;
  use_vmthreads = false;
  recursive_types = false;
  principal = false;
  transparent_modules = false;
  unboxed_types = false;
  unsafe_string = false;
  cookies = [];
}]

let () =
  assert (Bytestring.empty = Bytestring.empty);
  let all = Bytestring.empty in
  let len = Bytestring.empty in
  let body = Bytestring.empty in
  let _str = all in
  let _str = all in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_utf8 _trns all;
    Bytestring.Transient.commit _trns
  in
  let _str = all in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_string _trns ~size:10 all;
    Bytestring.Transient.commit _trns
  in
  let all = 2112 in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_bits _trns ~size:8 all;
    Bytestring.Transient.commit _trns
  in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_string _trns ~size:1 len;
    Bytestring.Transient.add_string _trns ~size:10 body;
    Bytestring.Transient.commit _trns
  in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_bits _trns ~size:1024 all;
    Bytestring.Transient.commit _trns
  in
  let all =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_string _trns ~size:123 len;
    Bytestring.Transient.commit _trns
  in
  let one = 2112 in
  let _str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_bits _trns ~size:8 one;
    Bytestring.Transient.add_string _trns all;
    Bytestring.Transient.commit _trns
  in
  let fin = 0 in
  let comp = 0 in
  let mask = 2112 in
  let payload =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_literal_utf8 _trns "this is my data";
    Bytestring.Transient.commit _trns
  in
  let rest =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_literal_string _trns "here's the rest";
    Bytestring.Transient.commit _trns
  in
  let len = 9000 in
  let str =
    let _trns = Bytestring.Transient.create () in
    Bytestring.Transient.add_bits _trns ~size:1 fin;
    Bytestring.Transient.add_bits _trns ~size:1 comp;
    Bytestring.Transient.add_literal_int _trns ~size:2 0;
    Bytestring.Transient.add_literal_int _trns ~size:4 1;
    Bytestring.Transient.add_literal_int _trns ~size:1 0;
    Bytestring.Transient.add_literal_int _trns ~size:7 127;
    Bytestring.Transient.add_bits _trns ~size:(8 * 8) len;
    Bytestring.Transient.add_bits _trns ~size:32 mask;
    Bytestring.Transient.add_string _trns ~size:len payload;
    Bytestring.Transient.add_string _trns rest;
    Bytestring.Transient.commit _trns
  in
  let compute_bits () = 8 * 8 in
  let _ =
    (fun _data_src ->
      try
        let _data_src = Bytestring.to_iter _data_src in
        let fin = Bytestring.Iter.next_bits ~size:1 _data_src in
        let comp = Bytestring.Iter.next_bits ~size:1 _data_src in
        try
          Bytestring.Iter.expect_literal_int _data_src ~size:2 0;
          Bytestring.Iter.expect_literal_int _data_src ~size:4 1;
          Bytestring.Iter.expect_literal_int _data_src ~size:1 0;
          Bytestring.Iter.expect_literal_int _data_src ~size:7 127;
          try
            let len =
              Bytestring.Iter.next_bits ~size:(compute_bits ()) _data_src
            in
            let _mask = Bytestring.Iter.next_bits ~size:32 _data_src in
            let _payload = Bytestring.Iter.next_bytes ~size:len _data_src in
            let rest = Bytestring.Iter.rest _data_src in
            Bytestring.Iter.expect_empty _data_src;
            fin + comp
          with Bytestring.No_match -> (
            try
              Bytestring.Iter.expect_empty _data_src;
              fin + comp
            with Bytestring.No_match -> raise Bytestring.No_match)
        with Bytestring.No_match -> (
          try
            Bytestring.Iter.expect_empty _data_src;
            fin + comp
          with Bytestring.No_match -> raise Bytestring.No_match)
      with Bytestring.No_match -> (
        try
          let rest = _data_src in
          Bytestring.length rest
        with Bytestring.No_match -> raise Bytestring.No_match))
      str
  in
  ()
