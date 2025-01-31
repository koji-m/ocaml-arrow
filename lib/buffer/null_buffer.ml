type t = { buffer : Boolean_buffer.t }

let create n = { buffer = Boolean_buffer.create n }

let of_buffer b len offset =
  { buffer = Boolean_buffer.{ buffer = b; len; offset } }

let is_null b i = if Boolean_buffer.get b.buffer i then false else true
let set b i = Boolean_buffer.set b.buffer i
let as_buffer b = Boolean_buffer.as_buffer b.buffer
let null_count b = Boolean_buffer.count_zero b.buffer
let sub b offset len = { buffer = Boolean_buffer.sub b.buffer offset len }
