open Arrow_array
open Arrow_buffer
open Arrow_schema
module FbMessageRt = Arrow_ipc_gen.Message.Rt
module FbMessage = Arrow_ipc_gen.Message.Org.Apache.Arrow.Flatbuf

exception NotSupported
exception Unexpected of string

let continuation_marker = Bytes.make 4 '\xff'

module Reader = struct
  type t = { fd : Unix.file_descr }

  let create fd = { fd }

  let read_exact rdr b =
    let len = Bytes.length b in
    Unix.read rdr.fd b 0 len
end

module Array_reader = struct
  type t = {
    version : FbMessage.MetadataVersion.t;
    data : Buffer.t;
    nodes : unit -> (int64 * int64) option;
    buffers : unit -> (int64 * int64) option;
  }

  let next_node ar = ar.nodes ()

  let next_buffer ar =
    match ar.buffers () with
    | Some (off, len) ->
        if len = 0L then None
        else
          Some
            (Buffer.sub ar.data ~off:(Int64.to_int off) ~len:(Int64.to_int len))
    | None -> None
end

let get_primitive_array_bufs reader =
  let null_buffer = Array_reader.next_buffer reader in
  let data_buffer =
    match Array_reader.next_buffer reader with
    | Some buf -> buf
    | None -> raise (Unexpected "data buffer not found")
  in
  let node =
    match Array_reader.next_node reader with
    | Some node -> node
    | _ -> raise (Unexpected "node not found")
  in
  let len, _ = node in
  (null_buffer, data_buffer, Int64.to_int len)

let create_array reader field =
  let data_type = Field.type_ field in
  match data_type with
  | Datatype.Int32 ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr = Primitive_array.Int32_array.make len data_buffer null_buffer in
      Array_intf.Array ((module Primitive_array.Int32_array), arr)
  | Datatype.Int64 ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr = Primitive_array.Int64_array.make len data_buffer null_buffer in
      Array_intf.Array ((module Primitive_array.Int64_array), arr)
  | Datatype.Float64 ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr =
        Primitive_array.Float64_array.make len data_buffer null_buffer
      in
      Array_intf.Array ((module Primitive_array.Float64_array), arr)
  | Datatype.Date32 ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr = Primitive_array.Date32_array.make len data_buffer null_buffer in
      Array_intf.Array ((module Primitive_array.Date32_array), arr)
  | Datatype.Date64 ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr = Primitive_array.Date64_array.make len data_buffer null_buffer in
      Array_intf.Array ((module Primitive_array.Date64_array), arr)
  | Datatype.Time32 Second ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr =
        Primitive_array.Time32_second_array.make len data_buffer null_buffer
      in
      Array_intf.Array ((module Primitive_array.Time32_second_array), arr)
  | Datatype.Time32 Millisecond ->
      let null_buffer, data_buffer, len = get_primitive_array_bufs reader in
      let arr =
        Primitive_array.Time32_millisecond_array.make len data_buffer
          null_buffer
      in
      Array_intf.Array ((module Primitive_array.Time32_millisecond_array), arr)
  | _ -> raise NotSupported

let read_record_batch buf (b, rb) ver schema_ =
  let bufs =
    FbMessage.RecordBatch.buffers b rb
    |> FbMessageRt.Option.get
    |> FbMessage.Buffer.Vector.to_seq b
    |> Seq.map (fun elt ->
           (FbMessage.Buffer.offset b elt, FbMessage.Buffer.length b elt))
  in
  let nodes =
    FbMessage.RecordBatch.nodes b rb
    |> FbMessageRt.Option.get
    |> FbMessage.FieldNode.Vector.to_seq b
    |> Seq.map (fun elt -> FbMessage.FieldNode.(length b elt, null_count b elt))
  in
  let array_reader =
    Array_reader.
      {
        version = ver;
        data = buf;
        nodes = Seq.to_dispenser nodes;
        buffers = Seq.to_dispenser bufs;
      }
  in
  let children =
    Array.map (fun f -> create_array array_reader f) (Schema.fields schema_)
  in
  Record_batch.{ schema = schema_; columns = children }

let fb_to_int_field b fb name_ =
  let bit_width = FbMessage.Int.bit_width b fb |> Int32.to_int in
  let signed = FbMessage.Int.is_signed b fb in
  match (bit_width, signed) with
  | 32, true -> Field.{ type_ = Datatype.Int32; name = name_ }
  | 64, true -> Field.{ type_ = Datatype.Int64; name = name_ }
  | _ -> raise Datatype.NotSupported

let fb_to_float_field b fb name_ =
  let precision = FbMessage.FloatingPoint.precision b fb in
  if precision = FbMessage.Precision.double then
    Field.{ type_ = Datatype.Float64; name = name_ }
  else raise Datatype.NotSupported

let fb_to_date_field b fb name_ =
  let date_unit = FbMessage.Date.unit b fb in
  if date_unit = FbMessage.DateUnit.millisecond then
    Field.{ type_ = Datatype.Date64; name = name_ }
  else if date_unit = FbMessage.DateUnit.day then
    Field.{ type_ = Datatype.Date32; name = name_ }
  else raise Datatype.NotSupported

let fb_to_time_field b fb name_ =
  let time_unit = FbMessage.Time.unit b fb in
  if time_unit = FbMessage.TimeUnit.second then
    Field.{ type_ = Datatype.Time32 Second; name = name_ }
  else if time_unit = FbMessage.TimeUnit.millisecond then
    Field.{ type_ = Datatype.Time32 Millisecond; name = name_ }
  else raise Datatype.NotSupported

let fb_to_field b fb_field =
  let name =
    FbMessage.Field.name b fb_field
    |> FbMessageRt.Option.get
    |> FbMessageRt.String.to_string b
  in
  FbMessage.Field.type_
    ~int:(fun fb -> fb_to_int_field b fb name)
    ~floating_point:(fun fb -> fb_to_float_field b fb name)
    ~date:(fun fb -> fb_to_date_field b fb name)
    ~time:(fun fb -> fb_to_time_field b fb name)
    ~default:(fun _typ -> raise Datatype.NotSupported)
    b fb_field

let fb_to_schema b fb_schema =
  let fields_ =
    FbMessage.Schema.fields b fb_schema
    |> FbMessageRt.Option.get
    |> FbMessage.Field.Vector.to_array b
    |> Array.map (fun elt -> fb_to_field b elt)
  in
  Schema.{ fields = fields_ }

module StreamReader = struct
  type t = { reader : Reader.t; schema : Schema.t }

  exception SchemaError

  let create reader =
    let meta_size = Bytes.create 4 in
    let _ = Reader.read_exact reader meta_size in
    let _ =
      if meta_size = continuation_marker then Reader.read_exact reader meta_size
      else 0
    in
    let meta_len = Bytes.get_int32_le meta_size 0 |> Int32.to_int in
    let meta_buffer = Bytes.create meta_len in
    let _ = Reader.read_exact reader meta_buffer in

    let (Root (b, msg)) =
      FbMessage.Message.root ~size_prefixed:false Flatbuffers.Primitives.Bytes
        meta_buffer
    in
    let schema =
      FbMessage.Message.header
        ~schema:(fun schm -> fb_to_schema b schm)
        ~default:(fun _ -> raise SchemaError)
        b msg
    in
    { reader; schema }

  let next sr =
    let meta_size = Bytes.create 4 in
    let _ = Reader.read_exact sr.reader meta_size in
    let _ =
      if meta_size = continuation_marker then
        Reader.read_exact sr.reader meta_size
      else 0
    in
    let meta_len = Bytes.get_int32_le meta_size 0 |> Int32.to_int in
    let meta_buffer = Bytes.create meta_len in
    if Reader.read_exact sr.reader meta_buffer > 0 then
      let (Root (b, msg)) =
        FbMessage.Message.root ~size_prefixed:false Flatbuffers.Primitives.Bytes
          meta_buffer
      in
      let header_type =
        FbMessage.(Message.header_type b msg |> MessageHeader.to_string)
      in
      match header_type with
      | "record_batch" ->
          let body_length = FbMessage.Message.body_length b msg in
          let body = Bytes.create (Int64.to_int body_length) in
          let _ = Reader.read_exact sr.reader body in
          let buffer = Buffer.of_bytes body in
          let ver = FbMessage.Message.version b msg in
          FbMessage.Message.header
            ~record_batch:(fun rb ->
              Some (read_record_batch buffer (b, rb) ver sr.schema))
            ~none:None
            ~default:(fun _ -> None)
            b msg
      | _ -> raise NotSupported
    else None
end

let read_stream f fd =
  let reader = Reader.create fd in
  let stream_reader = StreamReader.create reader in
  let rec loop () =
    match StreamReader.next stream_reader with
    | Some batch ->
        f batch;
        loop ()
    | None -> ()
  in
  loop ()
