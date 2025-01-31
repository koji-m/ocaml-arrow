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
        Some
          (Buffer.sub ar.data ~off:(Int64.to_int off) ~len:(Int64.to_int len))
    | None -> None
end

let create_array reader field =
  let data_type = Field.type_ field in
  match data_type with
  | Datatype.Int32 ->
      let null_buffer =
        match Array_reader.next_buffer reader with
        | Some buf -> buf
        | None -> raise (Unexpected "null buffer not found")
      in
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
      let arr =
        Primitive_array.Int32_array.make (Int64.to_int len) data_buffer
          null_buffer
      in
      Array_intf.Array ((module Primitive_array.Int32_array), arr)
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

let fb_to_field b fb_field =
  let f_type = FbMessage.(Field.type_type b fb_field |> Type.to_string) in
  let name_ =
    FbMessage.Field.name b fb_field
    |> FbMessageRt.Option.get
    |> FbMessageRt.String.to_string b
  in
  match f_type with
  | "int" -> Field.{ type_ = Datatype.Int32; name = name_ }
  | _ -> raise Datatype.NotSupported

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
