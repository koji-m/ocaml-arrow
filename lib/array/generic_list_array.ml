open Arrow_buffer
open Arrow_data
open Arrow_schema

module Offset_buffer =
  Offset_buffer.Offset_buffer (Arrow_primitive_types.Int32_type)

type t = {
  data_type : Datatype.t;
  value_offsets : Offset_buffer.t;
  values : Array_intf.t;
  nulls : Null_buffer.t option;
}

let of_int32_array_array aa =
  let num_slots = Array.length aa in
  let offsets =
    Array.map (fun a -> match a with Some a -> Array.length a | None -> 0) aa
    |> Offset_buffer.from_length
  in
  let values =
    Array.fold_left
      (fun acc a -> match a with Some a -> Array.append acc a | None -> acc)
      [||] aa
    |> Primitive_array.Int32_array.of_array
  in
  let nulls = Null_buffer.create num_slots in
  Array.iteri
    (fun i a -> match a with Some _ -> Null_buffer.set nulls i | None -> ())
    aa;
  {
    data_type = Datatype.List Field.{ type_ = Datatype.Int32; name = "inner" };
    value_offsets = offsets;
    values = Array_intf.Array ((module Primitive_array.Int32_array), values);
    nulls = Some nulls;
  }

let data_type a = a.data_type
let length a = Offset_buffer.length a.value_offsets - 1
let values a = a.value_offsets.buffer (* ToDo *)
let nulls a = a.nulls

let to_data a =
  let data_type_ = a.data_type in
  let values = a.values in
  let offsets = a.value_offsets.buffer in
  let nulls_ = a.nulls in
  let len = length a in
  Array_data.
    {
      data_type = data_type_;
      buffers = [| offsets |];
      nulls = nulls_;
      child_data = [| Array_intf.to_data values |];
      len;
      offset = 0;
    }
