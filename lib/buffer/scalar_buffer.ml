open Arrow_schema

type t = {buffer : Buffer.t; native_type :Native_type.t}

exception Ill_typed

let of_buffer b t =
  let native_t = match t with
  | Datatype.Int32 -> Native_type.Int32
  | _ -> raise Ill_typed
  in
  {buffer = b; native_type = native_t}
let length b = Buffer.length b.buffer / Native_type.size_of b.native_type

let get_int32 b i =
  Buffer.get_int32 b.buffer (i * 4)
