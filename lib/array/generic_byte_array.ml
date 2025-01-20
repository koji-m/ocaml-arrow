open Arrow_buffer
open Arrow_data
open Arrow_schema

type t = {
  data_type : Datatype.t;
  value_offsets : Offset_buffer.t;
  value_data : Buffer.t;
  nulls : Null_buffer.t option
}

let of_string_array a =
    let num_slots = Array.length a in
    let offsets = Array.map (fun s -> match s with Some s -> String.length s | None -> 0) a
        |> Offset_buffer.from_length in
    let values = Array.fold_left (fun acc s -> match s with Some s -> acc ^ s | None -> acc) "" a
        |> Buffer.of_string in
    let nulls = Null_buffer.create num_slots in
    Array.iteri (fun i s -> match s with Some _ -> Null_buffer.set nulls i | None -> ()) a;
    {
        data_type = Datatype.Utf8;
        value_offsets = offsets;
        value_data = values;
        nulls = Some nulls;
    }

let data_type a = a.data_type

let length a = Offset_buffer.length a.value_offsets - 1

let values a = a.value_data (* ToDo *)

let nulls a = a.nulls

let to_data a = 
  let data_type_ = a.data_type in
  let data = a.value_data in
  let offsets = a.value_offsets.buffer in
  let nulls_ = a.nulls in
  let len = length a in
  Array_data.({
    data_type = data_type_;
    buffers = [| offsets; data |];
    nulls = nulls_;
    child_data = [||];
    len = len;
    offset = 0 
  })