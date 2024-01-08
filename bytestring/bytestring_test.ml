let keyword fmt = Spices.(default |> fg (color "#00FF00") |> build) fmt
let error fmt = Spices.(default |> fg (color "#FF0000") |> build) fmt

let () =
  let test ?(print_expected = true) name fn expected =
    let actual =
      match fn () with
      | exception exn -> Format.sprintf "Exception: %s" (Printexc.to_string exn)
      | str -> str
    in
    let name =
      if not print_expected then Format.sprintf "%S <input skipped>" name
      else Format.sprintf "%S = %S" name expected
    in
    if String.equal actual expected then
      Format.printf "test %s %s\r\n%!" name (keyword "OK")
    else (
      Format.printf "\n%s\n\nExpected:\n\n%S\n\nbut found:\n\n%S\n\n"
        (error "Test %s failed" name)
        expected actual;
      assert false)
  in

  let proptest ?(count = 100) name gen fn =
    match
      let test = QCheck.Test.make ~count ~name gen fn in
      QCheck.Test.check_exn test;
      Format.printf "prop %S %s\r\n%!" name (keyword "OK")
    with
    | exception exn ->
        let exn = Format.sprintf "Exception: %s" (Printexc.to_string exn) in
        Format.printf "\n%s\n\n%s" (error "Test %S failed with:" name) exn;
        assert false
    | () -> ()
  in

  List.iteri
    (fun idx naughty_string ->
      let print_expected =
        match idx with
        | 96 | 97 | 98 | 178 | 162 | 163 | 164 | 165 -> false
        | _ -> true
      in
      test ~print_expected "naughty_string"
        (fun () -> Bytestring.of_string naughty_string |> Bytestring.to_string)
        naughty_string)
    Naughty_strings.strings;

  test {|concat (empty, ["a"; empty])|}
    (fun () ->
      List.map Bytestring.of_string [ "a"; "" ]
      |> Bytestring.(concat (of_string ""))
      |> Bytestring.to_string)
    "a";

  test {|concat (empty, ["a"; "1"])|}
    (fun () ->
      List.map Bytestring.of_string [ "a"; "1" ]
      |> Bytestring.(concat (of_string ""))
      |> Bytestring.to_string)
    "a1";

  test {|concat (empty, ["a"])|}
    (fun () ->
      List.map Bytestring.of_string [ "a" ]
      |> Bytestring.(concat (of_string ""))
      |> Bytestring.to_string)
    "a";

  test {|concat (empty, ["a"; "b"; empty])|}
    (fun () ->
      List.map Bytestring.of_string [ "a"; "b"; "" ]
      |> Bytestring.(concat (of_string ""))
      |> Bytestring.to_string)
    "ab";

  test {|concat ("a", ["`"; empty])|}
    (fun () ->
      List.map Bytestring.of_string [ "`"; "" ]
      |> Bytestring.(concat (of_string "a"))
      |> Bytestring.to_string)
    "`a";

  test {|sub (1, 0, "")|}
    (fun () -> Bytestring.(of_string "" |> sub ~off:1 ~len:0 |> to_string))
    "Exception: Bytestring.View_out_of_bounds";

  proptest "sub"
    QCheck.(tup3 int int string)
    (fun (min, max, str) ->
      (* don't go out of bounds *)
      let max = Int.max 0 (Int.min max (String.length str - 1)) in
      let min = Int.max 0 min in
      if min < max then
        let bytestring =
          Bytestring.(of_string str |> sub ~off:min ~len:max |> to_string)
        in
        String.equal (String.sub str min max) bytestring
      else true);

  proptest "join"
    QCheck.(tup2 string string)
    (fun (a, b) ->
      let b1 = Bytestring.of_string a in
      let b2 = Bytestring.of_string b in
      String.equal (a ^ b) Bytestring.(join b1 b2 |> to_string));

  proptest "join operator (^)"
    QCheck.(tup2 string string)
    (fun (a, b) ->
      let b1 = Bytestring.of_string a in
      let b2 = Bytestring.of_string b in
      String.equal (a ^ b) Bytestring.(b1 ^ b2 |> to_string));

  proptest "concat"
    QCheck.(tup2 string (list string))
    (fun (sep, strings) ->
      let bytestrings =
        List.map Bytestring.of_string strings
        |> Bytestring.(concat (of_string sep))
        |> Bytestring.to_string
      in
      let strings = String.concat sep strings in
      String.equal strings bytestrings);

  proptest "string roundtrip" QCheck.string (fun str ->
      String.equal Bytestring.(of_string str |> to_string) str);

  proptest "non-empty-string roundtrip"
    (QCheck.string_of_size (QCheck.Gen.int_range 1 100_000))
    (fun str -> String.equal Bytestring.(of_string str |> to_string) str);

  ()
