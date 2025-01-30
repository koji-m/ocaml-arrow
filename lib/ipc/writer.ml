open Arrow_buffer
open Arrow_schema
open Arrow_data
open Arrow_array

module FbMessageRt = Arrow_ipc_gen.Message.Rt
module FbMessage = Arrow_ipc_gen.Message.Org.Apache.Arrow.Flatbuf
module FbFileRt = Arrow_ipc_gen.File.Rt
module FbFile = Arrow_ipc_gen.File.Org.Apache.Arrow.Flatbuf

module Writer = struct
  type t = {fd : Unix.file_descr}

  let create fd = { fd = fd }

  let write_exact wrtr b =
    let len = Bytes.length b in
    Unix.write wrtr.fd b 0 len
end

let arrow_magic = Dynarray.of_array [|'A'; 'R'; 'R'; 'O'; 'W'; '1'|]
let continuation = Dynarray.make 4 '\xff'
let pad_to_alignment alignment len =
    let a = alignment - 1 in
    ((len + a) land (lnot a)) - len

module File = struct
    let schema_to_fb_offset schema b =
        let open Schema in
        let fields = Array.map (fun field -> Field.File.build_field b field) schema.fields in
        let fb_field_list = FbFile.Field.Vector.create b fields  in
        FbFile.Schema.Builder.(
            start b
            |> add_fields fb_field_list
            |> finish
        )
end

module Message = struct (* ToDo: same implementation as Schema.File. need to be fixed *)
    let schema_to_fb_offset schema b =
        let open Schema in
        let fields = Array.map (fun field -> Field.Message.build_field b field) schema.fields in
        let fb_field_list = FbMessage.Field.Vector.create b fields  in
        FbMessage.Schema.Builder.(
            start b
            |> add_fields fb_field_list
            |> finish
        )

    let schema_to_bytes schema =
        let b = FbMessageRt.Builder.create () in
        let fb_schema = schema_to_fb_offset schema b in
        let message = FbMessage.Message.Builder.(
            start b
            |> add_version FbMessage.MetadataVersion.v5
            |> add_header_schema fb_schema
            |> add_body_length 0L
            |> finish
        ) in
        let fb_bytes = FbMessage.Message.finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed:false b message in
        (
            (Bytes.to_seq fb_bytes |> Dynarray.of_seq),
            Dynarray.create ()
        )
end

let continuation_bytes len =
    let b = Buffer.create ~init:'\xff' 8 in
    Buffer.set_int32 b 4 len;
    b

type buffer_spec =
| FixedWidth of {byte_width : int; alignment : int}
| VariableWidth
| AlwaysNull

module DataTypeLayout = struct
    type t = {
        buffers : buffer_spec array;
        (* can_contain_null_mask : bool;
        variadic : bool *)
    }

    let make_fixed_width byte_width alignment = {
        buffers = [| FixedWidth {byte_width; alignment} |];
        (* can_contain_null_mask = true;
        variadic = false *)
    }

    let make_variable_width byte_width alignment = {
        buffers = [|
            FixedWidth {byte_width; alignment};
            VariableWidth;
        |]
    }
end

let layout data_type =
    match data_type with
    | Datatype.Int32 -> DataTypeLayout.make_fixed_width 4 64
    | Datatype.Float64 -> DataTypeLayout.make_fixed_width 8 64
    | Datatype.Utf8 -> DataTypeLayout.make_variable_width 4 64
    | _ -> raise Datatype.NotSupported

let get_buffer_element_size spec =
    match spec with
    | FixedWidth {byte_width; _} -> byte_width
    | _ -> 0

let buffer_need_truncate offset buffer spec min_length =
    spec != AlwaysNull && (offset != 0 || min_length < Buffer.length buffer)

let reencode_offset offsets (array_data : Array_data.t) =
    let slot_width = 4 in (* offset value byte width for Binary type and Utf8 type. ToDo: address 64bit offset value *)
    let offset_byte = array_data.offset * slot_width in
    let len_byte = array_data.len * slot_width in
    let offset_slice = Buffer.sub offsets ~off:offset_byte ~len:(len_byte + slot_width) in
    let start_offset = Buffer.get_int32 offset_slice 0 in
    let end_offset = Buffer.get_int32 offset_slice len_byte in
    let offsets = match start_offset with
    | 0l -> offset_slice
    | _ -> let new_offsets = Buffer.copy offset_slice ~off:0 ~len:(len_byte + slot_width) in
        let rec iter_i32 offsets =
            if Buffer.length offsets < 1 then
                ()
            else
                Buffer.set_int32 offsets 0 (Int32.sub (Buffer.get_int32 offsets 0) start_offset);
                iter_i32 (Buffer.sub offsets ~off:slot_width ~len:((Buffer.length offsets) - slot_width))
            in
        let () = iter_i32 new_offsets in
        new_offsets in
    (offsets, Int32.to_int start_offset, Int32.to_int (Int32.sub end_offset start_offset))

let get_value_buffers array_data =
    if Array_data.is_empty array_data then
        [Buffer.create 0; Buffer.create 0]
    else
        let offsets, original_start_offset, len = reencode_offset (array_data.buffers.(0)) array_data in
        let values = Buffer.sub array_data.buffers.(1) ~off:original_start_offset ~len:len in
        [offsets; values]

let get_offsets_and_child array_data =
    if Array_data.is_empty array_data then
        (Buffer.create 0, Array_data.empty ())
    else
        let offsets, original_start_offset, len = reencode_offset (array_data.buffers.(0)) array_data in
        let child = Array_data.sub array_data.child_data.(0) original_start_offset len in
        (offsets, child)

let get_children array_data =
    if Array_data.is_empty array_data then
        [||]
    else
        array_data.child_data



let write_buffer buffer buffers arrow_data offset _compression_codec alignment =
    let buf_data = Buffer.to_bytes buffer |> Bytes.to_seq |> Dynarray.of_seq in
    let () = Dynarray.append arrow_data buf_data in 
    let len = Buffer.length buffer in
    let buffers = Array.append buffers [|(offset, Int64.of_int len)|] in
    let pad_len = pad_to_alignment alignment len in
    let () = Dynarray.append arrow_data (Dynarray.make pad_len '\x00') in
    (Int64.add offset (Int64.of_int (len + pad_len)), buffers)

let rec write_array_data (array_data : Array_data.t) buffers arrow_data nodes offset num_rows null_count compression_codec alignment =
    let nodes = Array.append nodes [|(Int64.of_int num_rows, Int64.of_int null_count)|] in
    let null_buffer = match array_data.nulls with
        | Some null_buffer -> Null_buffer.as_buffer null_buffer
        | None -> Buffer.create ~init:'\xff' (Bit_util.ceil num_rows 8)
    in
    let offset, buffers = write_buffer null_buffer buffers arrow_data offset compression_codec alignment in
    let offset, buffers, nodes = match array_data.data_type with
    | Datatype.Int32
    | Datatype.Float64 -> 
        let buffer = array_data.buffers.(0) in
        let layout = layout array_data.data_type in
        let spec = layout.buffers.(0) in
        let byte_width = get_buffer_element_size spec in
        let min_length = array_data.len * byte_width in
        let buffer_to_write = if buffer_need_truncate array_data.offset buffer spec min_length then
            let byte_offset = array_data.offset * byte_width in
            let buffer_length = min min_length (Buffer.length buffer - byte_offset) in
            Buffer.sub buffer ~off:byte_offset ~len:buffer_length
        else
            buffer
        in
        let offset, buffers = write_buffer buffer_to_write buffers arrow_data offset compression_codec alignment in
        offset, buffers, nodes
    | Datatype.Utf8 ->
        let value_buffers = get_value_buffers array_data in
        let acc = (offset, buffers) in
        let offset, buffers = List.fold_left (fun (offset, buffers) vb ->
            write_buffer vb buffers arrow_data offset compression_codec alignment) acc value_buffers in
        offset, buffers, nodes
    | Datatype.List _ ->
        let offsets, child = get_offsets_and_child array_data in
        let offset, buffers = write_buffer offsets buffers arrow_data offset compression_codec alignment in
        let num_rows = Array_data.length child in
        let null_count = Array_data.null_count child in
        let offset, buffers, nodes = write_array_data child buffers arrow_data nodes offset num_rows null_count compression_codec alignment in
        offset, buffers, nodes
    | Datatype.Struct _ ->
        Array.fold_left
            (fun (offset, buffers, nodes) child ->
                let num_rows = Array_data.length child in
                let null_count = Array_data.null_count child in
                write_array_data child buffers arrow_data nodes offset num_rows null_count compression_codec alignment)
            (offset, buffers, nodes)
            array_data.child_data
    | Datatype.Boolean ->
        let buffer = array_data.buffers.(0) in
        let buffer = Bit_util.bit_sub buffer (Array_data.offset array_data) (Array_data.length array_data) in
        let offset, buffers = write_buffer buffer buffers arrow_data offset compression_codec alignment in
        offset, buffers, nodes
    in
    (offset, buffers, nodes)

let record_batch_to_bytes (batch : Record_batch.t) =
    let b = FbMessageRt.Builder.create () in

    let compression_codec = Some "LZ4" in
    let alignment = 64 in

    let arrow_data = Dynarray.create () in
    let write_array (offset, buffers, nodes) (column : Array_intf.t) =
        let Array_intf.Array((module A), arr) = column in
        let num_rows = A.length arr in
        let null_count = match A.nulls arr with
        | Some null_buf -> Null_buffer.null_count null_buf
        | None -> 0 in
        let array_data = A.to_data arr in
        write_array_data array_data buffers arrow_data nodes offset num_rows null_count compression_codec alignment
    in
    let _, buffers, nodes = Array.fold_left write_array (0L, [||], [||]) batch.columns in

    let buffers = FbMessage.Buffer.Vector.create b buffers  in
    let nodes = FbMessage.FieldNode.Vector.create b nodes in

    let batch_num_rows = Record_batch.num_rows batch |> Int64.of_int in
    let arrow_data_len = Dynarray.length arrow_data |> Int64.of_int in
    let record_batch = FbMessage.RecordBatch.Builder.(
        start b
        |> add_length batch_num_rows
        |> add_buffers buffers
        |> add_nodes nodes
        (* |> add_compression compression *)
        |> finish
    ) in
    let message = FbMessage.Message.Builder.(
        start b
        |> add_version FbMessage.MetadataVersion.v5
        |> add_header_record_batch record_batch
        |> add_body_length arrow_data_len
        |> finish
    ) in
    let fb_bytes = FbMessage.Message.finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed:false b message in
    (
        (Bytes.to_seq fb_bytes |> Dynarray.of_seq),
        arrow_data
    )

let write_message (ipc_message : char Dynarray.t) (body : char Dynarray.t) (arrow_data : char Dynarray.t) =
    let metadata_size_size = 8 in
    let a = 8 - 1 in
    let ipc_message_size = Dynarray.length ipc_message in
    let aligned_metadata_size = (ipc_message_size + metadata_size_size + a) land (lnot a) in
    let ipc_padding_bytes = aligned_metadata_size - ipc_message_size - metadata_size_size in 
    let metadata_size_bytes = Bytes.create 4 in
    let () = Bytes.set_int32_le metadata_size_bytes 0 (Int32.of_int (aligned_metadata_size - metadata_size_size)) in
    let metadata_size = Bytes.to_seq metadata_size_bytes |> Dynarray.of_seq in
    let body_length = Dynarray.length body in
    let body_padding_bytes = pad_to_alignment 8 body_length in
    Dynarray.append arrow_data continuation;
    Dynarray.append arrow_data metadata_size;
    Dynarray.append arrow_data ipc_message;
    Dynarray.append arrow_data (Dynarray.make ipc_padding_bytes '\x00');
    Dynarray.append arrow_data body;
    Dynarray.append arrow_data (Dynarray.make body_padding_bytes '\x00');
    (aligned_metadata_size, body_length + body_padding_bytes)

let to_footer_bytes (batch : Record_batch.t) (record_block : int * int * int) =
    let offset, metadata_size, arrow_data_size = record_block in
    let b = FbFileRt.Builder.create () in
    let dictionaries = FbFile.Block.Vector.create b [||] in
    let record_batches = FbFile.Block.Vector.create b [| (Int64.of_int offset, Int32.of_int metadata_size, Int64.of_int arrow_data_size)|] in
    let schema = File.schema_to_fb_offset batch.schema b in
    let footer = FbFile.Footer.Builder.(
        start b
        |> add_version FbFile.MetadataVersion.v5
        |> add_schema schema
        |> add_dictionaries dictionaries
        |> add_record_batches record_batches
        |> finish
    ) in
    let fb_bytes = FbFile.Footer.finish_buf Flatbuffers.Primitives.Bytes ~size_prefixed:false b footer in
    Bytes.to_seq fb_bytes |> Dynarray.of_seq

let build_schema_message arrow_data batch = 
    let ipc_message, message_body = Message.schema_to_bytes (Record_batch.schema batch) in
    write_message ipc_message message_body arrow_data

let build_record_batch_message arrow_data batch =
    let ipc_message, message_body = record_batch_to_bytes batch in
    write_message ipc_message message_body arrow_data

let append_eos_bytes arrow_data =    
    Dynarray.append arrow_data (Dynarray.make 4 '\xff');
    Dynarray.append arrow_data (Dynarray.make 4 '\x00')

let build_footer arrow_data batch record_block =
    let footer_bytes = to_footer_bytes batch record_block in
    let footer_size_bytes = Bytes.create 4 in
    Dynarray.length footer_bytes |> Int32.of_int |> Bytes.set_int32_le footer_size_bytes 0;
    Dynarray.append arrow_data footer_bytes;
    Dynarray.append arrow_data (Bytes.to_seq footer_size_bytes |> Dynarray.of_seq)

module StreamWriter = struct
  type t = {
    writer : Writer.t;
  }

  exception SchemaError
  
  let create writer = {writer = writer}
  
  let write sw batch =
    let arrow_data = Dynarray.create () in

    let _metadata_size, _arrow_data_size = build_schema_message arrow_data batch in
    let _metadata_size, _arrow_data_size = build_record_batch_message arrow_data batch in
    let () = append_eos_bytes arrow_data in
    Dynarray.to_seq arrow_data |> Bytes.of_seq |> Writer.write_exact sw.writer
end

module FileWriter = struct
  type t = {
    writer : Writer.t;
  }

  exception SchemaError

  let create writer = {writer = writer}

  let write fw batch =
    let arrow_data = Dynarray.create () in

    (* write magic and padding *)
    let magic_len = Dynarray.length arrow_magic in
    let magic_pad_len = pad_to_alignment 8 magic_len in
    let () = Dynarray.append arrow_data arrow_magic in
    Dynarray.append arrow_data (Dynarray.make magic_pad_len '\x00');
    let header_size = magic_len + magic_pad_len in

    let metadata_size, arrow_data_size = build_schema_message arrow_data batch in
    let block_offset = metadata_size + arrow_data_size + header_size in

    let metadata_size, arrow_data_size = build_record_batch_message arrow_data batch in
    let record_block = (block_offset, metadata_size, arrow_data_size) in

    let () = append_eos_bytes arrow_data in

    build_footer arrow_data batch record_block;

    (* write magic *)
    Dynarray.append arrow_data arrow_magic;

    Dynarray.to_seq arrow_data |> Bytes.of_seq |> Writer.write_exact fw.writer
end
