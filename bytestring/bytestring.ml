module Str = Stdlib.String
module Buf = Stdlib.Buffer

type t = { inner : string list; offset : int; length : int }

let empty = { inner = []; length = 0; offset = 0 }
let length t = t.length

exception Malformed of string

let of_string str =
  let length = Str.length str in
  let inner = [ str ] in
  { inner; length; offset = 0 }

let to_string t =
  if t.length = 0 then ""
  else
    let max_len = ref t.length in
    let buf = Bytes.create !max_len in
    let dst_pos = ref 0 in
    let start_off = ref 0 in
    let exception Done in
    (try
       for i = 0 to List.length t.inner - 1 do
         if !max_len = 0 then raise_notrace Done;
         let part = List.nth t.inner i in
         let len = Str.length part in

         if len = 0 then ()
         else if !start_off + len - 1 < t.offset then (
           start_off := !start_off + len;
           ())
         else
           let src_pos =
             if !start_off = t.offset then 0 else t.offset - !start_off
           in
           let src_pos = Int.max 0 src_pos in
           let len = Int.min !max_len (len - src_pos) in
           BytesLabels.blit_string ~src:part ~src_pos ~dst:buf ~dst_pos:!dst_pos
             ~len;
           dst_pos := !dst_pos + len;
           max_len := !max_len - len;
           start_off := !start_off + len
       done
     with Done -> ());
    Bytes.unsafe_to_string buf

exception View_out_of_bounds

let sub ?(off = 0) ~len t =
  if len + off > t.length then raise View_out_of_bounds;
  { t with offset = off; length = len }

let join s1 s2 =
  if s1.length = 0 then s2
  else if s2.length = 0 then s1
  else
    let length = s1.length + s2.length in
    let inner = s1.inner @ s2.inner in
    { inner; length; offset = s1.offset }

let ( ^ ) = join

let rec concat sep ls acc =
  match ls with
  | [] -> empty
  | h :: [] -> h
  | h :: t -> concat sep t ((h ^ sep) ^ acc)

let concat sep ls = concat sep ls empty

module Iter = struct
  exception Invalid_position
  exception Byte_not_found

  let get_bits t pos =
    let counter = ref pos in
    let exception Done of char in
    try
      for i = 0 to List.length t.inner - 1 do
        let part = List.nth t.inner i in
        let bits = Str.length part * 8 in
        for j = 0 to bits do
          if !counter = 0 then
            let byte = String.get part j in
            raise_notrace (Done byte)
          else counter := !counter - 1
        done
      done;
      raise_notrace Byte_not_found
    with Done byte -> byte

  let get_byte t pos =
    let counter = ref pos in
    let exception Done of char in
    try
      for i = 0 to List.length t.inner - 1 do
        let part = List.nth t.inner i in
        let len = Str.length part in
        for j = 0 to len - 1 do
          if !counter = 0 then
            let byte = String.get part j in
            raise_notrace (Done byte)
          else counter := !counter - 1
        done
      done;
      raise_notrace Byte_not_found
    with Done byte -> byte
end
