open Arrow_schema

module Offset_buffer (Apt : Arrow_primitive_types.Arrow_primitive_type) = struct
  module Scalar_buffer = Scalar_buffer.Scalar_buffer (Apt)

  type t = Scalar_buffer.t

  let from_length (a : int array) =
    let b = Scalar_buffer.create (Array.length a + 1) in
    let () = Scalar_buffer.set b 0 (Apt.native_to_bytes Apt.zero) in
    let _, _ =
      Array.fold_left
        (fun (i, sum) l ->
          let next = sum + l in
          Scalar_buffer.set b i Apt.(int_to_native next |> native_to_bytes);
          (i + 1, next))
        (1, 0) a
    in
    b

  let as_buffer b = Scalar_buffer.(b.buffer)
  let length a = Scalar_buffer.length a
end
