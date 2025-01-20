open Arrow_buffer
open Arrow_schema

type t = {
    data_type : Datatype.t;
    buffers : Buffer.t array;
    nulls : Null_buffer.t option;
    child_data : t array;
    len : int ;
    offset : int
}

let empty () =
    {
        data_type = Datatype.Int32;
        buffers = [||];
        nulls = None;
        child_data = [||];
        len = 0;
        offset = 0;
    }

let data_type a = a.data_type

let buffers a = a.buffers

let length a = a.len

let offset a = a.offset

let nulls a = a.nulls

let child_data a = a.child_data

let null_count a =
    match a.nulls with
    | Some b -> Null_buffer.null_count b
    | None -> 0

let is_empty a =
    a.len = 0

let sub a offset len =
    {
        data_type = a.data_type;
        buffers = a.buffers;
        nulls = a.nulls;
        child_data = a.child_data;
        len = len;
        offset = a.offset + offset
    }
