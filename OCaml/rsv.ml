module type HasOutput = sig
  type t

  val output : t -> bytes -> int -> int -> unit
  (** As with [Out_channel.output], [output t buf pos len] writes bytes
      [buf.[pos]] through [buf.[pos + len - 1]] to the output channel [t].
  *)
end

module type HasInput = sig
  type t

  val input : t -> bytes -> int -> int -> int
  (** As with [In_channel.input], [input t buf pos len] reads up to [len]
      bytes from the input channel [t] into [buf.[pos]] through
      [buf.[pos + len - 1]].  It returns the number of bytes read.
  *)
end

module type Foldable = sig
  type 'a t

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

let value_terminator = char_of_int 255
let null_value = char_of_int 254
let row_terminator = char_of_int 253

let _null_value_bytes =
  Bytes.init 2 (function 0 -> null_value | _ -> value_terminator)

let _row_terminator_bytes = Bytes.make 1 row_terminator

let _byte_class_lookup =
  [|
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1;
    2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
    3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3;
    4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
    4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
    0; 0; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
    5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
    6; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 8; 7; 7;
    9; 10; 10; 10; 11; 0; 0; 0; 0; 0; 0; 0; 0; 12; 13; 14;
  |]
[@ocamlformat "disable"]

let _state_transition_lookup =
  [|
    0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 1; 10; 11;
    0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 0; 0; 11;
    0; 0; 2; 2; 2; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 0; 0; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 3; 3; 3; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 0; 6; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 6; 6; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 6; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
    0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 11;
    0; 2; 0; 0; 0; 3; 4; 6; 5; 7; 8; 9; 1; 10; 11;
  |]
[@ocamlformat "disable"]

module Writer (Out : HasOutput) (Iter : Foldable) = struct
  module Internal = struct
    let iter f i = Iter.fold_left (fun () x -> f x) () i
  end

  let output_value out value =
    match value with
    | None -> Out.output out _null_value_bytes 0 2
    | Some s ->
        Out.output out (Bytes.unsafe_of_string s) 0 (String.length s);
        Out.output out _null_value_bytes 1 1

  let output_row out row =
    Internal.iter (output_value out) row;
    Out.output out _row_terminator_bytes 0 1

  let output_table out table = Internal.iter (output_row out) table
end

module Reader (In : HasInput) = struct
  module Internal = struct
    let read_chunk_size = 4096

    let read_more buf pos len in_ =
      let old_buf_len = len - pos in
      let next_buf_len = read_chunk_size + old_buf_len in
      let next_buf = Bytes.create next_buf_len in
      let readable_len =
        In.input in_ next_buf old_buf_len (next_buf_len - old_buf_len)
      in
      if readable_len = 0 then raise End_of_file
      else Bytes.blit buf pos next_buf 0 old_buf_len;
      (next_buf, old_buf_len + readable_len)

    let read_more_and_retry f buf pos len in_ =
      let next_buf, next_len = read_more buf pos len in_ in
      f next_buf 0 next_len in_
  end

  let rec read_value buf pos len in_ =
    let end_value_pos =
      try Bytes.index_from buf pos value_terminator with Not_found -> len
    in
    if end_value_pos >= len then
      Internal.read_more_and_retry read_value buf pos len in_
    else
      let decoded_value =
        if end_value_pos = pos + 1 && Bytes.get buf pos = null_value then None
        else Some (Bytes.sub_string buf pos (end_value_pos - pos))
      in
      (decoded_value, buf, end_value_pos + 1, len)

  let rec read_row acc rows buf pos len in_ =
    if pos >= len then
      Internal.read_more_and_retry (read_row acc rows) buf pos len in_
    else if Bytes.get buf pos = row_terminator then
      (List.rev acc :: rows, buf, pos + 1, len)
    else
      let value, buf, pos, len = read_value buf pos len in_ in
      read_row (value :: acc) rows buf pos len in_

  let rec read_table rows buf pos len in_ =
    if pos >= len then
      let buf, len =
        try Internal.read_more buf pos len in_ with End_of_file -> (buf, 0)
      in
      if len = 0 then List.rev rows else read_table rows buf 0 len in_
    else
      let rows, buf, pos, len = read_row [] rows buf pos len in_ in
      read_table rows buf pos len in_

  let read_all in_ =
    let buf = Bytes.create Internal.read_chunk_size in
    let len = In.input in_ buf 0 Internal.read_chunk_size in
    read_table [] buf 0 len in_

  let rec validate_all_rec state buf pos len in_ =
    if pos >= len then
      let buf, len =
        try Internal.read_more buf pos len in_ with End_of_file -> (buf, 0)
      in
      if len = 0 then state = 1 else validate_all_rec state buf 0 len in_
    else
      let current_byte = Bytes.get buf pos in
      let current_byte_class = _byte_class_lookup.(int_of_char current_byte) in
      let new_state_lookup_index = (state * 15) + current_byte_class in
      let new_state = _state_transition_lookup.(new_state_lookup_index) in
      if new_state = 0 then false
      else validate_all_rec new_state buf (pos + 1) len in_

  let validate_all in_ =
    let buf = Bytes.create Internal.read_chunk_size in
    let len = In.input in_ buf 0 Internal.read_chunk_size in
    validate_all_rec 1 buf 0 len in_
end

module Encoder (Iter : Foldable) =
  Writer
    (struct
      type t = Buffer.t

      let output = Buffer.add_subbytes
    end)
    (Iter)

module Decoder = struct
  include Reader (struct
    type t = bytes

    let input _in _out _pos _len = 0
  end)

  let read_all in_ = read_table [] in_ 0 (Bytes.length in_) in_
  let validate_all in_ = validate_all_rec 1 in_ 0 (Bytes.length in_) in_
end
