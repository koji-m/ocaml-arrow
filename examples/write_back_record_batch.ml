let write_back_record_batch_to_stdout batch =
  let cols = Arrow_array.Record_batch.columns batch in
  let res_cols = Array.map
    (fun col ->
      let i32_array = Arrow_array.Primitive_array.Int32_array.(as_array col |> to_array) in
      let res = Array.map
        (function
          | Some i -> Some (Int32.mul i i)
          | None -> None)
        i32_array in
      let res_array = Arrow_array.Primitive_array.Int32_array.of_array res in
      Arrow_array.Array_intf.Array((module Arrow_array.Primitive_array.Int32_array), res_array))
    cols in
  let res_batch = Arrow_array.Record_batch.({
    schema = Arrow_array.Record_batch.schema batch;
    columns = res_cols;
  }) in
  let sw = Arrow_ipc.Writer.Writer.create Unix.stdout
    |> Arrow_ipc.Writer.StreamWriter.create in
  let _ = Arrow_ipc.Writer.StreamWriter.write sw res_batch in
  () in

Arrow_ipc.Reader.read_stream write_back_record_batch_to_stdout Unix.stdin
