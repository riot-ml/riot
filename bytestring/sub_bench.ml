open Criterion

let str =
  List.init 100 (fun _ ->
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
|})
  |> String.concat ""

let binstr = Bytestring.of_string str

let str_full () =
  let _sum = String.sub str 0 (String.length str) in
  Ok ()

let str_half () =
  let _sum = String.sub str 0 (String.length str / 2) in
  Ok ()

let str_10 () =
  let _sum = String.sub str 0 10 in
  Ok ()

let str_100 () =
  let _sum = String.sub str 0 100 in
  Ok ()

let binstr_full () =
  let _sum = Bytestring.sub ~len:(Bytestring.length binstr) binstr in
  Ok ()

let binstr_half () =
  let _sum = Bytestring.sub ~len:(Bytestring.length binstr / 2) binstr in
  Ok ()

let binstr_10 () =
  let _sum = Bytestring.sub ~len:10 binstr in
  Ok ()

let binstr_100 () =
  let _sum = Bytestring.sub ~len:100 binstr in
  Ok ()

let () =
  let binstring_suite =
    Suite.
      {
        name = "binstring-suite";
        benches =
          [
            Bench.make ~max_runs:1_000_000 ~name:"str_full" str_full;
            Bench.make ~max_runs:1_000_000 ~name:"str_half" str_half;
            Bench.make ~max_runs:1_000_000 ~name:"str_10" str_10;
            Bench.make ~max_runs:1_000_000 ~name:"str_100" str_100;
            Bench.make ~max_runs:1_000_000 ~name:"binstr_full" binstr_full;
            Bench.make ~max_runs:1_000_000 ~name:"binstr_half" binstr_half;
            Bench.make ~max_runs:1_000_000 ~name:"binstr_10" binstr_10;
            Bench.make ~max_runs:1_000_000 ~name:"binstr_100" binstr_100;
          ];
      }
  in
  let runner = Runner.make ~suites:[ binstring_suite ] in
  Runner.run runner ~reporter:Reporter.cli
