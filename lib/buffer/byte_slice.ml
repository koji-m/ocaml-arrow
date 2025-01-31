type t = { data : bytes ref; offset : int; len : int }

let create ?(init = '\x00') n =
  { data = ref (Bytes.make n init); offset = 0; len = n }

let get b i = Bytes.get !(b.data) (b.offset + i)
let set b i c = Bytes.set !(b.data) (b.offset + i) c
let length b = b.len
let sub b ~off ~len = { data = b.data; offset = b.offset + off; len }
