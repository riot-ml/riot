module Str = Stdlib.String
module Buf = Stdlib.Buffer

type t = { inner : string list; offset : int; length : int }

let empty = { inner = []; length = 0; offset = 0 }
let length t = t.length

exception Malformed of string

let of_string str =
  let length = Str.length str in
  let buf = Bytes.create length in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let pos = ref 0 in
  let add char =
    let n = Bytes.set_utf_8_uchar buf !pos char in
    pos := !pos + n
  in
  for _i = 0 to length - 1 do
    match Uutf.decode decoder with
    | `Uchar char -> add char
    | `Malformed reason ->
        raise_notrace (Malformed (Printf.sprintf "%S" reason))
    | `Await -> ()
    | `End -> ()
  done;
  let inner = [ Bytes.unsafe_to_string buf ] in
  { inner; length; offset = 0 }

let to_string t =
  if t.length = 0 then ""
  else
    let max_len = ref (t.length - t.offset) in
    let buf = Bytes.create !max_len in
    let dst_pos = ref 0 in
    let start_off = ref 0 in
    let exception Done in
    (try
       for i = 0 to List.length t.inner - 1 do
         if !max_len = 0 then raise_notrace Done;
         let part = List.nth t.inner i in
         let len = Str.length part in

         if !start_off + len < t.offset then (
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
           max_len := !max_len - len
       done
     with Done -> ());
    Bytes.unsafe_to_string buf

exception View_out_of_bounds

let sub ?(off = 0) ~len t =
  if len - off > t.length then raise View_out_of_bounds;
  { t with offset = off; length = len }

let join s1 s2 =
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
