let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

module Random = struct
  let cstruct n = Mirage_crypto_rng.generate n
  let int8 () = Randomconv.int8 cstruct
  let int16 () = Randomconv.int16 cstruct
  let int32 () = Randomconv.int32 cstruct
  let int64 () = Randomconv.int64 cstruct
  let int ?max () = Randomconv.int ?bound:max cstruct
  let float ?max () = Randomconv.float ?bound:max cstruct
  let bytes n = cstruct n |> Cstruct.to_bytes
  let bigarray n = cstruct n |> Cstruct.to_bigarray
  let string n = cstruct n |> Cstruct.to_string
  let bytestring n = string n |> Bytestring.of_string
end
