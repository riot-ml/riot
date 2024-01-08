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

  test {|add_literal_utf trns "\xc3\x28" raises|}
    (fun () ->
      let trn = Bytestring.(to_transient empty) in
      Bytestring.Transient.add_literal_utf8 trn "\xc3\x28";
      "")
    "Exception: Bytestring.Transient.Invalid_utf8(\"\\195(\")";

  proptest "add literal utf8 string" (fun input ->
      let str = Bytestring.of_string input in
      let trn = Bytestring.to_transient str in
      Bytestring.Transient.add_literal_utf8 trn "🦮 ";
      let str = Bytestring.Transient.commit trn in
      let output = Bytestring.to_string str in
      String.equal (input ^ "🦮 ") output);

  proptest "add literal string" (fun input ->
      let str = Bytestring.of_string input in
      let trn = Bytestring.to_transient str in
      Bytestring.Transient.add_literal_string trn "hello";
      let str = Bytestring.Transient.commit trn in
      let output = Bytestring.to_string str in
      String.equal (input ^ "hello") output);

  proptest "add to empty string" (fun input ->
      let str = Bytestring.of_string "" in
      let trn = Bytestring.to_transient str in
      Bytestring.Transient.add_string trn (Bytestring.of_string input);
      let str = Bytestring.Transient.commit trn in
      let output = Bytestring.to_string str in
      String.equal input output);

  proptest "duplicate" (fun input ->
      let str = Bytestring.of_string input in
      let trn = Bytestring.to_transient str in
      Bytestring.Transient.add_string trn str;
      let str = Bytestring.Transient.commit trn in
      let output = Bytestring.to_string str in
      String.equal (input ^ input) output);

  ()
