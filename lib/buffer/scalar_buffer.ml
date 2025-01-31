open Arrow_schema

module Scalar_buffer (Apt : Arrow_primitive_types.Arrow_primitive_type) = struct
  type t = { buffer : Buffer.t }

  exception Ill_typed

  let create n =
    let len = n * Apt.byte_width in
    let buf = Buffer.create len in
    { buffer = buf }

  let of_buffer b = { buffer = b }
  let length b = Buffer.length b.buffer / Apt.byte_width

  let get b i =
    let byte_width = Apt.byte_width in
    Buffer.sub b.buffer ~off:(i * byte_width) ~len:byte_width |> Buffer.to_bytes

  let set b offset byts =
    let offset = offset * Apt.byte_width in
    Bytes.iteri (fun i c -> Buffer.set b.buffer (offset + i) c) byts
end
