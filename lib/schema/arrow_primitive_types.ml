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
  let bytes_to_native (b : bytes) = Bytes.get_int32_le b 0
end

module Int64_type : Arrow_primitive_type with type native_t = int64 = struct
  type native_t = int64

  let byte_width = 8
  let data_type = Datatype.Int64
  let int_to_native i = Int64.of_int i

  let native_to_bytes (i : native_t) =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 i;
    b

  let zero = 0L
  let bytes_to_native (b : bytes) = Bytes.get_int64_le b 0
end

module Float64_type : Arrow_primitive_type with type native_t = float = struct
  type native_t = float

  let byte_width = 8
  let data_type = Datatype.Float64
  let int_to_native i = Float.of_int i

  let native_to_bytes (f : native_t) =
    let f = Int64.bits_of_float f in
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 f;
    b

  let zero = Float.zero

  let bytes_to_native (b : bytes) =
    Bytes.get_int64_le b 0 |> Int64.float_of_bits
end

module Date64_type : Arrow_primitive_type with type native_t = int64 = struct
  type native_t = int64

  let byte_width = 8
  let data_type = Datatype.Date64
  let int_to_native i = Int64.of_int i

  let native_to_bytes (i : native_t) =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 i;
    b

  let zero = 0L
  let bytes_to_native (b : bytes) = Bytes.get_int64_le b 0
end

