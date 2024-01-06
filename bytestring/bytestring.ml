module Str = Stdlib.String
module Buf = Stdlib.Buffer

type t = { inner : string list; offset : int; length : int }

let empty = { inner = []; length = 0; offset = 0 }
let length t = t.length

exception No_match
exception Guard_mismatch
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

let view ?(off = 0) ~len t =
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
  type string = t
  type t = I

  exception Invalid_position
  exception Byte_not_found

  let next_bit _t = 0
  let next_bits ~size:_ _t = 0
  let next_byte _t = empty
  let next_bytes ~size:_ _t = empty
  let next_utf8 _t = empty
  let next_utf8_seq ~len:_ _t = empty
  let rest _t = empty
  let expect_bits _bit _t = ()
  let expect_bytes _bytes _t = ()
  let expect_literal_int _t ?size:_ _bit = ()
  let expect_literal_string _t ?size:_ _str = ()
  let expect_empty _t = ()
end

let to_iter _t = Iter.I

module Transient = struct
  type string = t
  type t = T

  let add_bits _t ?size:_ _str = ()
  let add_literal_int _t ?size:_ _str = ()
  let add_literal_string _t ?size:_ _str = ()
  let add_literal_utf8 _t ?size:_ _str = ()
  let add_string _t ?size:_ _str = ()
  let add_utf8 _t ?size:_ _utf8 = ()
  let commit _t = empty
  let create () = T
end

let to_transient _t = Transient.T
