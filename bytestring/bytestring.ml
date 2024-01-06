module Str = Stdlib.String
module Buf = Stdlib.Buffer

exception No_match
exception Guard_mismatch
exception Malformed of string
exception View_out_of_bounds

module Rep = struct
  type range = { offset : int; length : int }
  type t = Flat of string | Chunked of range * string list

  (* Chunked representation invariants:

     - ChunkedInv1: [off >= 0 && len >= 0]

     [off] and [len] are non-negative.

     - ChunkedInv2: [List.empty parts => off = 0 && len = 0]

     If there are no [parts], then [off] and [len] must be zero.

     - ChunkedInv3: [off < String.length (List.hd parts)]

     [off] is never equal or greater than the length of the first part.
     If that were the case, then the first part would have no reason to exist.
     This also means that [List.hd parts] is never an empty string.

     - ChunkedInv4: [off + len <= sum (List.map String.length parts)]

     [off + len] is never greater than the sum of the lengths of all parts.


     All the mutable operations on [t] must preserve these invariants, and
     consenquently all the immutable operations can assume them.
  *)

  (** If a sub-string is smaller than this threshold, then the cost of copying
      such a slice is smaller than the cost of creating a more complicated
      representation and processing it.

      NOTE(felipecrv): this value is arbitrary and needs to be tuned based on
      practical benchmarks.
    *)
  let cheap_sub_threshold = 16

  (** [blit_string_chunks ~src_chunks ~dst ~dst_pos ~len] copies [len] bytes
      from [src_chunks] to [dst] starting at [dst_pos]. The source starting
      point is the beginning of the first chunk.

      [blit_string_chunks] doesn't handle source offsets. This enable a clean
      recursive implementation and the skipping a bunch of checks after the
      caller has taken care of the first potentially offset chunk.

      Pre-conditions:
        [~dst_pos >= 0]

        [~len > 0] because it shouldn't be called if there isn't work to do.

        [~dst_pos + ~len <= Bytes.length dst] because the caller is expected to
        have allocated enough space for the blits.

        [sum (List.map String.length src_chunks) >= ~len] because there must be
        enough bytes in the source chunks to satisfy the blit.
   *)
  let rec blit_string_chunks ~src_chunks ~dst ~dst_pos ~len =
    assert (dst_pos >= 0 && len > 0);
    assert (dst_pos + len <= Bytes.length dst);
    match src_chunks with
    | [] -> failwith "sum (List.map String.length ~src_chunks) < ~len"
    | first :: rest ->
        let first_len = String.length first in
        if len <= first_len then Bytes.blit_string first 0 dst dst_pos len
        else (
          Bytes.blit_string first 0 dst dst_pos first_len;
          let len' = len - first_len in
          assert (len' > 0);
          blit_string_chunks ~src_chunks:rest ~dst
            ~dst_pos:(dst_pos + first_len) ~len)
end

type t = Rep.t

let empty = Rep.Flat ""

let length = function
  | Rep.Flat s -> String.length s
  | Rep.Chunked ({ length; _ }, _) -> length

let of_string str = Rep.Flat str

let to_string = function
  | Rep.Flat s -> s
  | Rep.Chunked ({ offset; length }, parts) -> (
      match parts with
      | [] -> ""
      | first :: rest ->
          let buf = Bytes.create length in
          let first_tail_len = Str.length first - offset in
          (* ChunkedInv1 and ChunkedInv3 imply that we always need a non-zero
             amount of bytes from the first chunk. *)
          assert (first_tail_len > 0);
          (* NOTE(felipecrv): we can skip many branch instructions by using
             Bytes.unsafe_blit_string here as long as we can prove
             all the pre-conditions from the invariants. *)
          if length <= first_tail_len then (
            Bytes.blit_string first offset buf 0 length;
            Bytes.unsafe_to_string buf)
          else (
            Bytes.blit_string first offset buf 0 first_tail_len;
            let len' = length - first_tail_len in
            assert (len' > 0);
            Rep.blit_string_chunks ~src_chunks:rest ~dst:buf
              ~dst_pos:first_tail_len ~len:len';
            Bytes.unsafe_to_string buf))

let view ?(off = 0) ~len t =
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
        else
          (* All the Chunked representation invariants are satisfied due to
             all the checks above. We know len is non-zero based on the check
             against Rep.cheap_sub_threshold. And since len is valid, we know s
             is non-empty. *)
          Rep.Chunked ({ offset = off; length = len }, [ s ])
    | Rep.Chunked (_, _) -> failwith "TODO: implement view for chunked strings"

(* TODO(felipcrv): continue from here *)
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
