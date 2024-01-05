open Criterion

let str =
  {|A modern-day warrior
Mean, mean stride
Today's Tom Sawyer
Mean, mean pride

Though his mind is not for rent
Don't put him down as arrogant
His reserve a, quiet defense
Riding out the day's events
The river

What you say about his company
Is what you say about society
Catch the mist, catch the myth
Catch the mystery, catch the drift

The world is, the world is
Love and life are deep
Maybe as his skies are wide

Today's Tom Sawyer, he gets high on you
And the space he invades, he gets by on you

No, his mind is not for rent
To any god or government
Always hopeful, yet discontent
He knows changes aren't permanent
But changes is

And what you say about his company
Is what you say about society
Catch the witness, catch the wit
Catch the spirit, catch the spit

The world is, the world is
Love and life are deep
Maybe as his eyes are wide

Exit the warrior, today's Tom Sawyer
He gets high on you and the energy you trade
He gets right onto the friction of the day!
|}

let strings =
  [|
    List.init 1 (fun _k -> str);
    List.init 10 (fun _k -> str);
    List.init 100 (fun _k -> str);
    List.init 1_000 (fun _k -> str);
    List.init 10_000 (fun _k -> str);
    List.init 100_000 (fun _k -> str);
    List.init 1_000_000 (fun _k -> str);
  |]

let binstrings = Array.map (List.map Bytestring.of_string) strings

let str_make_and_fold n () =
  let _sum = String.concat "" strings.(n - 1) in
  Ok ()

let binstr_make_and_fold n () =
  let _sum = Bytestring.concat Bytestring.empty binstrings.(n - 1) in
  Ok ()

let () =
  let binstring_suite =
    Suite.
      {
        name = "binstring-suite";
        benches =
          [
            Bench.make ~name:"str.concat 1" (str_make_and_fold 1);
            Bench.make ~name:"bin.concat 1" (binstr_make_and_fold 1);
            Bench.make ~name:"str.concat 10" (str_make_and_fold 2);
            Bench.make ~name:"bin.concat 10" (binstr_make_and_fold 2);
            Bench.make ~name:"str.concat 100" (str_make_and_fold 3);
            Bench.make ~name:"bin.concat 100" (binstr_make_and_fold 3);
            Bench.make ~name:"str.concat 1k" (str_make_and_fold 4);
            Bench.make ~name:"bin.concat 1k" (binstr_make_and_fold 4);
            Bench.make ~name:"str.concat 10k" (str_make_and_fold 5);
            Bench.make ~name:"bin.concat 10k" (binstr_make_and_fold 5);
            Bench.make ~name:"str.concat 100k" (str_make_and_fold 6);
            Bench.make ~name:"bin.concat 100k" (binstr_make_and_fold 6);
            Bench.make ~name:"bin.concat 1M" (binstr_make_and_fold 7);
          ];
      }
  in
  let runner = Runner.make ~suites:[ binstring_suite ] in
  Runner.run runner ~reporter:Reporter.cli
