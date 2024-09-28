let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

module Random = struct
  let string n = Mirage_crypto_rng.generate n
  let int8 () = Randomconv.int8 string
  let int16 () = Randomconv.int16 string
  let int32 () = Randomconv.int32 string
  let int64 () = Randomconv.int64 string
  let char () = Char.chr (int8 ())
  let int ?max () = Randomconv.int ?bound:max string
  let float ?max () = Randomconv.float ?bound:max string
  let bytes n = string n |> Bytes.unsafe_of_string
  let bytestring n = string n |> Bytestring.of_string
  let alphanum () = Char.chr (48 + Randomconv.int ~bound:74 string)
  let seq n gen = List.init n (fun _ -> gen ()) |> List.to_seq |> String.of_seq
end
