open Arrow_buffer
open Arrow_data
open Arrow_schema

type t = {
  data_type : Datatype.t;
  values : Scalar_buffer.t;
  nulls : Null_buffer.t option
}

let of_int32_array a =
    let byte_width = 4 in
    let num_slots = Array.length a in
    let len = num_slots * byte_width in
    let data_buf = Buffer.create len in
    let null_buf = Null_buffer.create num_slots in
    Array.iteri (
        fun i v -> match v with
        | Some v ->
            Null_buffer.set null_buf i;
            Buffer.set_int32 data_buf (i * byte_width) v
        | None -> ()
    ) a;
    {
        data_type = Datatype.Int32;
        values = Scalar_buffer.of_buffer data_buf Datatype.Int32;
        nulls = Some null_buf;
    }

let data_type a = a.data_type

let length a = Scalar_buffer.length a.values

let values a = a.values.buffer

let nulls a = a.nulls

let to_data a = 
  let buffer = a.values.buffer in
  let data_type_ = a.data_type in
  let nulls_ = a.nulls in
  let len = length a in
  Array_data.({
    data_type = data_type_;
    buffers = [|buffer|];
    nulls = nulls_;
    child_data = [||];
    len = len;
    offset = 0 
  })

exception PrimitiveArrayError of string

let of_array (arr : Array_intf.t) =
  let Array_intf.Array((module A), a) = arr in
  let data_type_ = A.data_type a in
  match data_type_ with
  | Datatype.Int32 ->
    {
      data_type = data_type_;
      values = Scalar_buffer.of_buffer (A.values a) data_type_;
      nulls = A.nulls a;
    }
  | _ -> raise (PrimitiveArrayError "can not convert to PrimitiveArray")

let to_int32_array arr =
  match data_type arr with
  | Datatype.Int32 ->
    let buffer = arr.values in
    let nulls = match arr.nulls with
    | Some nulls -> nulls
    | None -> raise (PrimitiveArrayError "no null buffer") in
    let len = length arr in
    let i32_arr = Array.make len None in
    Array.mapi_inplace
      (fun i _ -> 
        if Null_buffer.is_null nulls i then
          None
        else
          Some (Scalar_buffer.get_int32 buffer i)
      )
      i32_arr;
    i32_arr
  | _ -> raise (PrimitiveArrayError "not int32 array")