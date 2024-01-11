let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

let () =
  let test name fn expected =
    let actual =
      match fn () with
      | exception exn -> Format.sprintf "Exception: %s" (Printexc.to_string exn)
      | str -> str
    in
    if String.equal actual expected then
      Format.printf "test %S %s\r\n%!" name (keyword "OK")
    else (
      Format.printf "\n%s\n\nExpected:\n\n%S\n\nbut found:\n\n%S\n\n"
        (error "Test %S failed" name)
        expected actual;
      assert false)
  in

  let non_empty_string ?(min = 3) ?(max = 100_000) () =
    QCheck.string_of_size (QCheck.Gen.int_range min max)
  in
  let proptest ?(count = 1_000) ?(gen = fun () -> QCheck.string) name fn =
    match
      let test = QCheck.Test.make ~count ~name (gen ()) fn in
      QCheck.Test.check_exn test;
      Format.printf "prop %S %s\r\n%!" name (keyword "OK")
    with
    | exception exn ->
        Format.printf "\n%s\n\n" (error "Test %S failed with:" name);
        raise exn
    | () -> ()
  in

  test "create iterator and expect empty"
    (fun () ->
      let str = Bytestring.of_string "" in
      let iter = Bytestring.to_iter str in
      Bytestring.Iter.expect_empty iter;
      "")
    "";

  test "create iterator and expect empty to fail"
    (fun () ->
      let str = Bytestring.of_string "asdf" in
      let iter = Bytestring.to_iter str in
      Bytestring.Iter.expect_empty iter;
      "")
    "Exception: Bytestring.Iter.Invalid_position";

  test "create empty iterator and get 1 byte fails"
    (fun () ->
      let str = Bytestring.of_string "" in
      let iter = Bytestring.to_iter str in
      let _byte = Bytestring.Iter.next_byte iter in
      "")
    "Exception: Bytestring.Iter.Invalid_position";

  test "create empty iterator and get 10 byte fails"
    (fun () ->
      let str = Bytestring.of_string "" in
      let iter = Bytestring.to_iter str in
      let _byte = Bytestring.Iter.next_bytes ~size:10 iter in
      "")
    "Exception: Bytestring.Iter.Invalid_position";

  proptest "create iterator and rest" (fun input ->
      let str = Bytestring.of_string input in
      let iter = Bytestring.to_iter str in
      let rest = Bytestring.Iter.rest iter in
      let output = Bytestring.to_string rest in
      String.equal input output);

  proptest "create iterator and get 1 byte fails" ~gen:non_empty_string
    (fun input ->
      let str = Bytestring.of_string input in
      let iter = Bytestring.to_iter str in
      let byte = Bytestring.Iter.next_byte iter in
      let output = Bytestring.to_string byte in
      String.equal (String.sub input 0 1) output);

  proptest "create iterator and get many bytes one by one"
    ~gen:(non_empty_string ~min:3) (fun input ->
      let str = Bytestring.of_string input in
      let iter = Bytestring.to_iter str in
      let byte1 = Bytestring.Iter.rest iter in
      let output = Bytestring.to_string byte1 in
      String.equal input output);

  proptest "create iterator and get half the bytes"
    ~gen:(non_empty_string ~min:10 ~max:1000) (fun input ->
      let size = String.length input / 2 in
      let str = Bytestring.of_string input in
      let iter = Bytestring.to_iter str in
      let byte = Bytestring.Iter.next_bytes ~size iter in
      let output = Bytestring.to_string byte in
      String.equal (String.sub input 0 size) output);

  proptest "iterator preserves state" ~gen:(non_empty_string ~min:6)
    (fun input ->
      let str = Bytestring.of_string input in
      let iter = Bytestring.to_iter str in
      let byte = Bytestring.Iter.next_bytes ~size:5 iter in
      let rest = Bytestring.Iter.rest iter in
      let str = Bytestring.join byte rest in
      let output = Bytestring.to_string str in
      String.equal input output);

  ()
