module Str = Stdlib.String
module Buf = Stdlib.Buffer

exception No_match
exception Guard_mismatch
exception Malformed of string
exception View_out_of_bounds

module Rep = struct
  type range = { offset : int; length : int }

  type view = range * string
  (** A valid, non-empty, and strict sub-range with an associated string. *)

  let suffix_length suffix_opt =
    match suffix_opt with None -> 0 | Some ({ length; _ }, _) -> length

  type chunked = { parts : string list; length : int }
  (** A non-empty list of strings and a cached sum of all the lengths. *)

  (** Every operation should produce a representation in canonical form -- a
      form that can't be reduced by any reduction rule below:

      [View ({offset; length}, s)] can be reduced to [Flat s] if
      [offset=0 && length=String.length s]. A [view] should always be a,
      non-empty, and strict sub-range of the associated string.

      [View] can be reduced to [Flat ""] if [length=0].

      [Chunked ({parts = []; length = 0}, Some v)] can be reduced to [View v].

      [Chunked ({parts = []; length = 0}, None)] can be reduced to [Flat ""].

      [Chunked ({parts = [s]; _}, None)] can be reduced to [Flat s].

      [ChunkedWithOffset ((range, s) as view, parts, suffix)] can be reduced to
      [Chunked (s :: parts, suffix)] if [view] is empty or covers the entire range
      of the associated string.
    *)
  type t =
    | Flat of string
    | View of view
    | Chunked of chunked * view option
    | ChunkedWithOffset of view * chunked * view option

  (** If a sub-string is smaller than this threshold, then the cost of copying
      such a slice is smaller than the cost of creating a more complicated
      representation plus the future costs incurred when that complicated
      representation is processed.

      NOTE(felipecrv): this value is arbitrary and needs to be tuned based on
      practical benchmarks.
    *)
  let cheap_sub_threshold = 16

  let cheap_concat_threshold = 128
  let sum_lengths = List.fold_left (fun acc s -> acc + String.length s) 0

  (** [copy_string_chunks ~src_parts ~dst ~dst_pos ~len] copies all bytes
      from [src_parts] to [dst] starting at [dst_pos]. The source starting
      point is the beginning of the first chunk.

      Pre-conditions: see the asserts in the code.
   *)
  let rec copy_string_chunks ~src_parts ~dst ~dst_pos =
    assert (dst_pos >= 0);
    assert (dst_pos + sum_lengths src_parts <= Bytes.length dst);
    match src_parts with
    | [] -> ()
    | first :: rest ->
        let first_len = String.length first in
        Bytes.blit_string first 0 dst dst_pos first_len;
        copy_string_chunks ~src_parts:rest ~dst ~dst_pos:(dst_pos + first_len)

  (** Pre-conditions: see the asserts in the code. *)
  let copy_chunked_string ~chunked ~(suffix : view option) ~dst ~dst_pos =
    assert (dst_pos >= 0);
    assert (
      dst_pos + suffix_length suffix + sum_lengths chunked.parts
      <= Bytes.length dst);
    copy_string_chunks ~src_parts:chunked.parts ~dst ~dst_pos;
    match suffix with
    | None -> ()
    | Some (r, s) ->
        Bytes.blit_string s r.offset dst (dst_pos + chunked.length) r.length

  let join_string t1 s2 =
    if String.length s2 = 0 then t1
    else
      match t1 with
      | Flat s1 ->
          let length = String.length s1 + String.length s2 in
          if length <= cheap_concat_threshold then Flat (s1 ^ s2)
          else Chunked ({ parts = [ s1; s2 ]; length }, None)
      | View (({ offset; length }, s1) as view) ->
          let length' = length + String.length s2 in
          if length' <= cheap_concat_threshold then
            (* TODO: fuse sub and ^ for a single allocation *)
            let s1' = String.sub s1 offset length in
            Flat (s1' ^ s2)
          else if length <= cheap_sub_threshold then
            let s1' = String.sub s1 offset length in
            Chunked ({ parts = [ s1'; s2 ]; length = length' }, None)
          else
            ChunkedWithOffset
              (view, { parts = [ s2 ]; length = String.length s2 }, None)
      | _ -> failwith "TODO: implement join_string"

  let join_chunked t1 chunked suffix =
    failwith "TODO: implement join_chunked"

  (** Pre-conditions: off >= 0 && len >= 0 *)
  let sub_from_view ~view ~off ~len =
    assert (off >= 0 && len >= 0);
    match view with
    | { offset; length }, s ->
        if offset + off > length - len then raise View_out_of_bounds
        else if len <= cheap_sub_threshold then
          Flat (String.sub s (offset + off) len)
        else View ({ offset = offset + off; length = len }, s)

  let sub_from_chunked ~chunked ~suffix ~off ~len =
    failwith "TODO: implement sub_from_chunked"
end

type t = Rep.t

let empty = Rep.Flat ""

let length = function
  | Rep.Flat s -> String.length s
  | Rep.View ({ length; _ }, _) -> length
  | Rep.Chunked ({ length; _ }, suffix) -> length + Rep.suffix_length suffix
  | Rep.ChunkedWithOffset (prefix, { parts; length }, suffix) ->
      (fst prefix).length + length + Rep.suffix_length suffix

let is_empty t =
  match t with
  | Rep.Flat s -> String.length s = 0
  | Rep.View _ | Rep.Chunked _ | Rep.ChunkedWithOffset _ ->
      (* Bytestring.t is expected to be in canonical form and the canonical
         representation of the empty string is Flat. *)
      assert (length t > 0);
      false

let of_string str = Rep.Flat str

let to_string = function
  | Rep.Flat s -> s
  | Rep.View ({ offset; length }, s) -> String.sub s offset length
  | Rep.Chunked (chunked, suffix) ->
      let len = chunked.length + Rep.suffix_length suffix in
      let buf (* : local_ *) = Bytes.create len in
      Rep.copy_chunked_string ~chunked ~suffix ~dst:buf ~dst_pos:0;
      Bytes.unsafe_to_string buf
  | Rep.ChunkedWithOffset ((r, s), chunked, suffix) ->
      let len = r.length + chunked.length + Rep.suffix_length suffix in
      let buf (*: local_ *) = Bytes.create len in
      Rep.copy_chunked_string ~chunked ~suffix ~dst:buf ~dst_pos:r.length;
      Bytes.unsafe_to_string buf

let join s1 s2 =
  match s2 with
  | Rep.Flat s2' -> Rep.join_string s1 s2'
  | Rep.Chunked (chunked, suffix) -> Rep.join_chunked s1 chunked suffix
  | _ -> failwith "TODO: implement join"
(* TODO(felipcrv): continue from here *)

let ( ^ ) = join

let rec concat sep ls acc =
  match ls with
  | [] -> empty
  | h :: [] -> h
  | h :: t -> concat sep t ((h ^ sep) ^ acc)

let concat sep ls = concat sep ls empty

let sub ?(off = 0) ~len t =
  (* guard against negative offsets and lengths *)
  if off < 0 || len < 0 then raise View_out_of_bounds
  else
    match t with
    | Rep.Flat s ->
        let s_len = String.length s in
        if off > s_len - len then raise View_out_of_bounds
        else if off = 0 && len = s_len then t
        else if len <= Rep.cheap_sub_threshold then
          Rep.Flat (String.sub s off len)
        else Rep.View ({ offset = off; length = len }, s)
    | Rep.View view -> Rep.sub_from_view ~view ~off ~len
    | Rep.Chunked (chunked, suffix) ->
        Rep.sub_from_chunked ~chunked ~suffix ~off ~len
    | Rep.ChunkedWithOffset ((({ offset; length }, s) as view), chunked, suffix)
      ->
        if off >= length then
          (* skip the entire prefix *)
          Rep.sub_from_chunked ~chunked ~suffix ~off:(off - length) ~len
        else
          let new_prefix_len = Int.min (length - off) len in
          let new_prefix = Rep.sub_from_view ~view ~off ~len:new_prefix_len in
          let new_tail =
            Rep.sub_from_chunked ~chunked ~suffix ~off:0
              ~len:(len - new_prefix_len)
          in
          join new_prefix new_tail

let view = sub

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
