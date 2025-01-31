open Arrow_buffer
open Arrow_data
open Arrow_schema

type t = {
  data_type : Datatype.t;
  fields : Array_intf.t array;
  nulls : Null_buffer.t option;
}

let make fields arrays nulls =
  { data_type = Datatype.Struct fields; fields = arrays; nulls }

let data_type a = a.data_type

let length a =
  if Array.length a.fields > 0 then
    let (Array_intf.Array ((module A), a)) = a.fields.(0) in
    A.length a
  else 0

let values _a = Buffer.create 0 (* ToDo *)
let nulls a = a.nulls

let to_data a =
  let data_type_ = a.data_type in
  let fields =
    Array.map
      (fun arr ->
        let (Array_intf.Array ((module A), a)) = arr in
        A.to_data a)
      a.fields
  in
  let nulls_ = a.nulls in
  let len = length a in
  Array_data.
    {
      data_type = data_type_;
      buffers = [||];
      nulls = nulls_;
      child_data = fields;
      len;
      offset = 0;
    }
