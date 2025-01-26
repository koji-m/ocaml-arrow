module type Arrow_primitive_type = sig
  type native_t

  val byte_width : int

  val data_type : Datatype.t

  val int_to_native : int -> native_t

  val native_to_bytes : native_t -> bytes

  val bytes_to_native : bytes -> native_t

  val zero : native_t
end

module Int32_type : Arrow_primitive_type with type native_t = int32 = struct
  type native_t = int32

  let byte_width = 4

  let data_type = Datatype.Int32

  let int_to_native i = Int32.of_int i

  let native_to_bytes (i : native_t) =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 i;
    b

  let zero = 0l

  let bytes_to_native (b : bytes) =
    Bytes.get_int32_le b 0
end