open Arrow_buffer
open Arrow_data
open Arrow_schema

type t = {
  data_type : Datatype.t;
  values : Boolean_buffer.t;
  nulls : Null_buffer.t option;
}

let of_boolean_array ba =
  let len = Array.length ba in
  let bool_buf = Boolean_buffer.create len in
  let null_buf = Null_buffer.create len in
  let f i b =
    match b with
    | Some b ->
        if b then Boolean_buffer.set bool_buf i else ();
        Null_buffer.set null_buf i
    | None -> ()
  in
  Array.iteri f ba;
  { data_type = Datatype.Boolean; values = bool_buf; nulls = Some null_buf }

let data_type a = a.data_type
let length a = Boolean_buffer.length a.values
let values a = a.values.buffer (* ToDo *)
let nulls a = a.nulls

let to_data a =
  let buffer = a.values.buffer in
  let data_type_ = Datatype.Boolean in
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
