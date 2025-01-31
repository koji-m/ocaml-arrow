type t = { buffer : Buffer.t; len : int; offset : int }

let create n =
  { buffer = Buffer.create (Bit_util.ceil n 8); len = n; offset = 0 }

let length b = b.len

let get b i =
  let i = i + b.offset in
  let n_bytes = i / 8 in
  let check_bits =
    (Buffer.get b.buffer n_bytes |> Char.code) land (1 lsl (i mod 8))
  in
  if check_bits = 0 then false else true

let set b i =
  let i = i + b.offset in
  let n_bytes = i / 8 in
  let bit_set =
    (Buffer.get b.buffer n_bytes |> Char.code) lor (1 lsl (i mod 8))
  in
  Buffer.set b.buffer n_bytes (Char.chr bit_set)

let as_buffer b = b.buffer

let count_zero b =
  let num_bits_set =
    Buffer.fold_left
      (fun acc c -> acc + (Char.code c |> Z.of_int |> Z.popcount))
      b.buffer 0
  in
  b.len - num_bits_set

let sub b offset len = { buffer = b.buffer; len; offset = b.offset + offset }
