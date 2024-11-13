open Riot

type Message.t += ContentMatch of string | Error of string

let () =
  Riot.run_with_status ~on_error:(fun _ -> 1) @@ fun () ->
  let _ = Logger.start () in
  Logger.set_log_level (Some Debug);
  let main = self () in
  let proc_count = 10_000 in
  let _ =
    spawn (fun () ->
        for id = 0 to proc_count do
          spawn (fun () ->
              let dir = Filename.temp_dir "_riot_test" (Int.to_string id) in
              Sys.chdir dir;
              let filename = dir ^ "/" ^ "test_" ^ Int.to_string id in
              let content = filename in
              let () = File.write filename ~content |> Result.get_ok in
              let msg =
                match File.read_to_string filename with
                | Ok actual ->
                    if String.equal actual content then ContentMatch filename
                    else
                      Error
                        (Format.sprintf
                           "%d: the content did not match! %S != %S\n\
                            filename: %s" id actual content filename)
                | Error (`File_not_found file) ->
                    let cwd = Sys.getcwd () in
                    Error
                      (Format.sprintf
                         "%d: file not found: %S in %S because cwd is %S" id
                         file dir cwd)
                | Error (#IO.io_error as e) ->
                    let err = Format.asprintf "%d: error: %a" id IO.pp_err e in
                    Error err
              in
              send main msg)
          |> ignore
        done)
  in

  for _i = 0 to proc_count do
    match receive_any () with
    | ContentMatch _filename -> ()
    | Error str -> failwith str
    | _ -> ()
  done;
  Ok 0
