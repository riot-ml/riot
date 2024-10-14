let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

(* Some of the code from below is based on the randomconv repository.
   Source: https://github.com/hannesm/randomconv/
   The original code is licensed under the following license:

   Copyright (c) 2016 Hannes Mehnert hannes@mehnert.org

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.
   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

module Random = struct
  let string n = Mirage_crypto_rng.generate n
  let int8 () = Bytes.get_uint8 (Bytes.unsafe_of_string (string 1)) 0
  let int16 () = Bytes.get_uint16_le (Bytes.unsafe_of_string (string 2)) 0
  let int32 () = Bytes.get_int32_le (Bytes.unsafe_of_string (string 4)) 0
  let int64 () = Bytes.get_int64_le (Bytes.unsafe_of_string (string 8)) 0

  let bitmask n =
    let rec go c = function 0 -> c | n -> go (c lsl 1) (n lsr 1) in
    go 1 n - 1

  let rec int ?(max = max_int) () =
    if max <= 0 then invalid_arg "bound smaller or equal 0 not supported";
    if max = 1 then 0
    else
      let r =
        if max <= 256 then int8 ()
        else if max <= 65536 then int16 ()
        else
          match Sys.word_size with
          | 32 -> Int32.to_int (int32 ())
          | 64 -> Int64.to_int (int64 ())
          | _ -> invalid_arg "unknown word size"
      in
      let r = r land bitmask (pred max) in
      if r < max then r else int ~max ()

  let float ?(max = 1.) () =
    if max <= 0. then invalid_arg "bound smaller or equal 0 not supported";
    let scale = float_of_int max_int and r1 = int () and r2 = int () in
    max *. (((float_of_int r1 /. scale) +. float_of_int r2) /. scale)

  let char () = Char.chr (int8 ())
  let bytes n = string n |> Bytes.unsafe_of_string
  let bytestring n = string n |> Bytestring.of_string
  let alphanum () = Char.chr (48 + int ~max:74 ())
  let seq n gen = List.init n (fun _ -> gen ()) |> List.to_seq |> String.of_seq
end
