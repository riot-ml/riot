module Str = Stdlib.String
module Buf = Stdlib.Buffer

exception No_match
exception Guard_mismatch
exception Malformed of string
exception View_out_of_bounds

module Rep = struct
  type range = { offset : int; length : int }

  type view = range * string
  (** A valid sub-range with an associated string.

      When used inside representations, it should always be a, non-empty, and a
      strict sub-range of the associated string.
   *)

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

      [Chunked ({_; length = 0}, Some v)] can be reduced to [View v].

      [Chunked ({_; length = 0}, None)] can be reduced to [Flat ""].

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

  let cheap_join_threshold = 128
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

  let[@tail_mod_cons] rec fuse_last l ~f x =
    match l with
    | a :: [] -> ( match f a x with Some a' -> [ a' ] | None -> [ a; x ])
    | a :: l' -> a :: fuse_last l' ~f x
    | [] -> failwith "fuse_last: empty list"

  let join_views (v1 : view) (v2 : view) =
    let range1, s1 = v1 in
    let range2, s2 = v2 in
    let length = range1.length + range2.length in
    let buf = Bytes.create length in
    Bytes.blit_string s1 range1.offset buf 0 range1.length;
    Bytes.blit_string s2 range2.offset buf range1.length range2.length;
    Bytes.unsafe_to_string buf

  (** Join 2 strings if they are small enough or return a list containing both
      strings. *)
  let maybe_join s1 s2 =
    if String.length s1 + String.length s2 <= cheap_join_threshold then
      Some (s1 ^ s2)
    else None

  let maybe_join_views (v1 : view) (v2 : view) =
    if (fst v1).length + (fst v2).length <= cheap_join_threshold then
      Some (join_views v1 v2)
    else None

  let view_of_string s = ({ offset = 0; length = String.length s }, s)
  let view_to_string (r, s) = String.sub s r.offset r.length

  let join_string t1 s2 =
    let s2_length = String.length s2 in
    if s2_length = 0 then t1
    else
      match t1 with
      | Flat s1 ->
          let length = String.length s1 + s2_length in
          if length <= cheap_join_threshold then Flat (s1 ^ s2)
          else Chunked ({ parts = [ s1; s2 ]; length }, None)
      | View (({ offset; length }, s1) as view) ->
          let length' = length + s2_length in
          if length' <= cheap_join_threshold then
            Flat (join_views view (view_of_string s2))
          else if length <= cheap_sub_threshold then
            let s1' = String.sub s1 offset length in
            Chunked ({ parts = [ s1'; s2 ]; length = length' }, None)
          else
            ChunkedWithOffset
              (view, { parts = [ s2 ]; length = s2_length }, None)
      | Chunked (chunked, None) ->
          Chunked
            ( {
                parts = fuse_last chunked.parts s2 ~f:maybe_join;
                length = chunked.length + s2_length;
              },
              None )
      | Chunked (chunked, Some suffix) ->
          Chunked
            ( {
                parts = chunked.parts @ [ join_views suffix (view_of_string s2) ];
                length = chunked.length + (fst suffix).length + s2_length;
              },
              None )
      | ChunkedWithOffset (prefix, chunked, None) ->
          ChunkedWithOffset
            ( prefix,
              {
                parts = fuse_last chunked.parts s2 ~f:maybe_join;
                length = chunked.length + s2_length;
              },
              None )
      | ChunkedWithOffset (prefix, chunked, Some suffix) ->
          ChunkedWithOffset
            ( prefix,
              {
                parts =
                  chunked.parts @ [ join_views suffix (view_of_string s2) ];
                length = chunked.length + (fst suffix).length + s2_length;
              },
              None )

  let join_view t1 (v2 : view) =
    (* v2 is canonical, so it's non-empty *)
    let v2_length = (fst v2).length in
    assert (v2_length > 0);
    match t1 with
    | Flat s -> (
        let s_length = String.length s in
        if s_length = 0 then View v2
        else
          match maybe_join_views (view_of_string s) v2 with
          | Some s' -> Flat s
          | None -> Chunked ({ parts = [ s ]; length = s_length }, Some v2))
    | View v -> ChunkedWithOffset (v, { parts = []; length = 0 }, Some v2)
    | Chunked (chunked, None) -> Chunked (chunked, Some v2)
    | Chunked (chunked, Some suffix) ->
        let suffix' = join_views suffix v2 in
        Chunked
          ( {
              parts = chunked.parts @ [ suffix' ];
              length = chunked.length + String.length suffix';
            },
            None )
    | ChunkedWithOffset (prefix, chunked, None) ->
        ChunkedWithOffset (prefix, chunked, Some v2)
    | ChunkedWithOffset (prefix, chunked, Some suffix) ->
        let suffix' = join_views suffix v2 in
        ChunkedWithOffset
          ( prefix,
            {
              parts = chunked.parts @ [ suffix' ];
              length = chunked.length + String.length suffix';
            },
            None )

  let join_chunked t1 chunked2 suffix2 =
    match t1 with
    | Flat s ->
        let s_length = String.length s in
        if s_length = 0 then Chunked (chunked2, suffix2)
        else
          Chunked
            ( {
                parts = s :: chunked2.parts;
                length = s_length + chunked2.length;
              },
              suffix2 )
    | View v -> ChunkedWithOffset (v, chunked2, suffix2)
    | Chunked (chunked1, None) ->
        Chunked
          ( {
              parts = chunked1.parts @ chunked2.parts;
              length = chunked1.length + chunked2.length;
            },
            suffix2 )
    | Chunked (chunked1, Some suffix1) ->
        (* XXX: if suffix1 is small enough, then we could join it with
           the last chunk of chunked1 or the first chunk of chunked2, but this
           view materialization will do for now *)
        let suffix1' = view_to_string suffix1 in
        Chunked
          ( {
              parts = chunked1.parts @ [ suffix1' ] @ chunked2.parts;
              length = chunked1.length + (fst suffix1).length + chunked2.length;
            },
            suffix2 )
    | ChunkedWithOffset (prefix1, chunked1, None) ->
        ChunkedWithOffset
          ( prefix1,
            {
              parts = chunked1.parts @ chunked2.parts;
              length = chunked1.length + chunked2.length;
            },
            suffix2 )
    | ChunkedWithOffset (prefix1, chunked1, Some suffix1) ->
        (* XXX: if suffix1 is small enough, then we could join it with
           the last chunk of chunked1 or the first chunk of chunked2, but this
           view materialization will do for now *)
        let suffix1' = view_to_string suffix1 in
        ChunkedWithOffset
          ( prefix1,
            {
              parts = chunked1.parts @ [ suffix1' ] @ chunked2.parts;
              length = chunked1.length + (fst suffix1).length + chunked2.length;
            },
            suffix2 )

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
    (* XXX: remember to ensure the returned representation is in canonical form *)
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

let join t1 t2 =
  match t2 with
  | Rep.Flat s2 -> Rep.join_string t1 s2
  | Rep.View v2 -> Rep.join_view t1 v2
  | Rep.Chunked (chunked, suffix) -> Rep.join_chunked t1 chunked suffix
  | Rep.ChunkedWithOffset (prefix, chunked, suffix) ->
      let t1' = Rep.join_view t1 prefix in
      Rep.join_chunked t1' chunked suffix

let ( ^ ) = join

let rec concat sep ls acc =
  match ls with
  | [] -> empty
  | h :: [] -> acc ^ h
  | h :: t -> concat sep t (acc ^ h ^ sep)

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

(** TODO: there should be two types of Iter:
    - Iter that iteretes only on byte boundaries
    - BitIter that iterates on arbitrary bit boundaries

    This is necessary because the overhead of keeping non-byte aligned
    boundaries is too high for the common case of byte aligned boundaries.

    Imagine a request for multiple bytes that starts at a non-byte aligned
    boundary.
    *)
module Iter = struct
  type bytestring = t
  type t = { mutable length : int; mutable bytes : int Seq.t }

  exception Invalid_position
  exception Byte_not_found

  (* Iterators on bits *)

  let next_bit _t = 0
  let next_bits ~size:_ _t = 0
  let expect_bits _bit _t = ()

  (* Iterators on bytes *)

  let expect_bytes _bytes _t = ()
  let expect_literal_int _t ?size:_ _bit = ()
  let expect_literal_string _t ?size:_ _str = ()
  let expect_empty t = if t.length != 0 then raise Invalid_position

  let next_byte t =
    if t.length = 0 then raise Invalid_position;
    (* this is safe because we expect [t.length > 0] *)
    let byte, bytes = Seq.uncons t.bytes |> Option.get in
    t.length <- t.length - 1;
    t.bytes <- bytes;
    String.make 1 (Char.chr byte) |> of_string

  let next_bytes ~size t =
    if t.length = 0 || size > t.length then raise Invalid_position;
    let rec read bytes acc =
      if List.length acc = size then
        let read =
          List.rev acc |> List.map Char.chr |> List.to_seq |> String.of_seq
          |> of_string
        in
        (read, bytes)
      else
        (* this is safe because we expect [t.length > 0 && size <= t.length ] *)
        let byte, next = Seq.uncons bytes |> Option.get in
        read next (byte :: acc)
    in
    let read, bytes = read t.bytes [] in
    t.bytes <- bytes;
    t.length <- t.length - size;
    read

  let next_utf8 _t = empty
  let next_utf8_seq ~len:_ _t = empty

  (** [rest t] turns our iterator back into a bytestring *)
  let rest t =
    let rest = t.bytes |> Seq.map Char.chr |> String.of_seq |> of_string in
    t.bytes <- Seq.empty;
    t.length <- 0;
    rest

  let make string =
    let bytes = to_string string in
    let length = String.length bytes in
    let bytes = bytes |> String.to_seq |> Seq.map Char.code in
    { length; bytes }
end

let to_iter t = Iter.make t

module TransientRep = struct
  type t = { mutable store : Rep.t }
  (** Transient representations implement the CoW pattern.

      When they are created, they are just a reference to the source
      Bytestring representation. When a mutation happens, we copy parts of it
      to a mutable representation and perform the mutation on these
      mutation-friendly parts.

      When the transient representation is committed, we copy the mutated parts
      back to an immutable representation and return it.

      TODO(felipecrv): implement a more sophisticated version of Mut
      *)

  let from_source t = { store = t }
  let create () = { store = empty }
  let add_string t bs = t.store <- join t.store bs
  let add_literal_string t s = t.store <- Rep.join_string t.store s
  let commit t = t.store
end

module Transient = struct
  type bytestring = t
  type t = TransientRep.t

  let create = TransientRep.create

  let add_string t ?size str =
    let str = match size with None -> str | Some len -> sub ~len str in
    TransientRep.add_string t str

  let add_bits _t ?size:_ _str = ()
  let add_literal_int _t ?size:_ _str = ()
  let add_utf8 _t ?size:_ _utf8 = ()

  let add_literal_string t ?size str =
    let str =
      match size with None -> str | Some len -> String.sub str 0 len
    in
    TransientRep.add_literal_string t str

  exception Invalid_utf8 of string

  let add_literal_utf8 t ?size str =
    let str, size =
      match size with
      | None -> (str, String.length str)
      | Some len -> (String.sub str 0 len, len)
    in
    let decoder = Uutf.decoder (`String str) in
    let buf = Buffer.create size in
    let rec decode_all () =
      match Uutf.decode decoder with
      | `Uchar char ->
          Uutf.Buffer.add_utf_8 buf char;
          decode_all ()
      | `Malformed str -> raise (Invalid_utf8 str)
      | `Await -> ()
      | `End -> ()
    in
    decode_all ();
    add_literal_string t (Buffer.contents buf)

  let commit = TransientRep.commit
end

let to_transient = TransientRep.from_source
