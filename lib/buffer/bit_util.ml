let ceil n d = if n mod d > 0 then (n / d) + 1 else n / d

module Bitchunk = struct
  type t = {
    buffer : Buffer.t;
    bit_offset : int;
    chunk_len : int;
    remainder_len : int;
  }

  let of_buffer (buf : Buffer.t) offset len =
    let byte_offset = offset / 8 in
    let bit_offset = offset mod 8 in
    let chunk_len = len / 8 in
    let remainder_len = len mod 8 in
    {
      buffer =
        Buffer.sub buf ~off:byte_offset ~len:(Buffer.length buf - byte_offset);
      bit_offset;
      chunk_len;
      remainder_len;
    }

  let iteri f bc =
    let bit_offset = bc.bit_offset in
    let chunk_len = bc.chunk_len in
    let remainder_len = bc.remainder_len in
    let rec iteri_ f i =
      if i < chunk_len then (
        let current = Char.code (Buffer.get bc.buffer i) in
        let next = Char.code (Buffer.get bc.buffer (i + 1)) in
        let c =
          (current lsr bit_offset) lor (255 land (next lsl (8 - bit_offset)))
        in
        f i (Char.chr c);
        iteri_ f (i + 1))
      else if remainder_len > 0 then
        let current = Char.code (Buffer.get bc.buffer i) in
        let valid_bits = (1 lsl remainder_len) - 1 in
        let c = (current lsr bit_offset) land valid_bits in
        f i (Char.chr c)
      else ()
    in
    iteri_ f 0
end

let bit_sub left offset len =
  if offset mod 8 = 0 then
    Buffer.
      {
        data = left.data;
        offset = left.offset + (offset / 8);
        len = ceil len 8;
      }
  else
    (* Creates enough bytes to accommodate data of the given bit length and initializes the bits to 0. *)
    let result = Buffer.create ~init:'\x00' (ceil len 8) in
    (* let result_chunks = Bitchunk.of_buffer result in *)
    let left_chunks = Bitchunk.of_buffer left offset len in
    Bitchunk.iteri (fun i c -> Buffer.set result i c) left_chunks;
    result
