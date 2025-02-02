open Arrow_buffer
open Arrow_data
open Arrow_schema

module Primitive_array (Apt : Arrow_primitive_types.Arrow_primitive_type) =
struct
  module Scalar_buffer = Scalar_buffer.Scalar_buffer (Apt)

  type t = {
    data_type : Datatype.t;
    values : Scalar_buffer.t;
    nulls : Null_buffer.t option;
  }

  exception PrimitiveArrayError of string

  let make len data_buf null_buf =
    let null_buffer = Null_buffer.of_buffer null_buf len 0 in
    {
      data_type = Apt.data_type;
      values = Scalar_buffer.of_buffer data_buf;
      nulls = Some null_buffer;
    }

  let of_array (a : Apt.native_t option array) =
    let num_slots = Array.length a in
    let scalar_buf = Scalar_buffer.create num_slots in
    let null_buf = Null_buffer.create num_slots in
    Array.iteri
      (fun i v ->
        match v with
        | Some v ->
            let () = Null_buffer.set null_buf i in
            let b = Apt.native_to_bytes v in
            Scalar_buffer.set scalar_buf i b
        | None -> ())
      a;
    { data_type = Apt.data_type; values = scalar_buf; nulls = Some null_buf }

  let data_type a = a.data_type
  let length a = Scalar_buffer.length a.values
  let values a = a.values.buffer
  let nulls a = a.nulls

  let to_data a =
    let buffer = a.values.buffer in
    let data_type_ = a.data_type in
    let nulls_ = a.nulls in
    let len = length a in
    Array_data.
      {
        data_type = data_type_;
        buffers = [| buffer |];
        nulls = nulls_;
        child_data = [||];
        len;
        offset = 0;
      }

  let as_array (arr : Array_intf.t) =
    let (Array_intf.Array ((module A), a)) = arr in
    let data_type_ = A.data_type a in
    {
      data_type = data_type_;
      values = Scalar_buffer.of_buffer (A.values a);
      nulls = A.nulls a;
    }

  let to_array (arr : t) : Apt.native_t option array =
    let buffer = arr.values in
    let nulls =
      match arr.nulls with
      | Some nulls -> nulls
      | None -> raise (PrimitiveArrayError "no null buffer")
    in
    let len = length arr in
    let native_arr = Array.make len None in
    Array.mapi_inplace
      (fun i _ ->
        if Null_buffer.is_null nulls i then None
        else Some (Scalar_buffer.get buffer i |> Apt.bytes_to_native))
      native_arr;
    native_arr
end

module Int32_array = Primitive_array (Arrow_primitive_types.Int32_type)
module Int64_array = Primitive_array (Arrow_primitive_types.Int64_type)
module Float64_array = Primitive_array (Arrow_primitive_types.Float64_type)
