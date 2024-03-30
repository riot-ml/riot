let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

module Random = struct
  let cstruct n = Mirage_crypto_rng.generate n
  let cstruct_to_string n = Mirage_crypto_rng.generate n |> Cstruct.to_string
  let int8 () = Randomconv.int8 cstruct_to_string
  let int16 () = Randomconv.int16 cstruct_to_string
  let int32 () = Randomconv.int32 cstruct_to_string
  let int64 () = Randomconv.int64 cstruct_to_string
  let char () = Char.chr (int8 ())
  let int ?max () = Randomconv.int ?bound:max cstruct_to_string
  let float ?max () = Randomconv.float ?bound:max cstruct_to_string
  let bytes n = cstruct n |> Cstruct.to_bytes
  let bigarray n = cstruct n |> Cstruct.to_bigarray
  let string n = cstruct n |> Cstruct.to_string
  let bytestring n = string n |> Bytestring.of_string
  let alphanum () = Char.chr (48 + Randomconv.int ~bound:74 cstruct_to_string)
  let seq n gen = List.init n (fun _ -> gen ()) |> List.to_seq |> String.of_seq
end
