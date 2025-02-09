let write_back_record_batch_to_stdout batch =
  let cols = Arrow_array.Record_batch.columns batch in

  let col = cols.(0) in
  let i32_array =
    Arrow_array.Primitive_array.Int32_array.(as_array col |> to_array)
  in
  let res =
    Array.map
      (function Some i -> Some (Int32.mul i i) | None -> None)
      i32_array
  in
  let res_array = Arrow_array.Primitive_array.Int32_array.of_array res in
  let res_i32_col =
    Arrow_array.Array_intf.Array
      ((module Arrow_array.Primitive_array.Int32_array), res_array)
  in

  let col = cols.(1) in
  let i64_array =
    Arrow_array.Primitive_array.Int64_array.(as_array col |> to_array)
  in
  let res =
    Array.map
      (function Some i -> Some (Int64.mul i i) | None -> None)
      i64_array
  in
  let res_array = Arrow_array.Primitive_array.Int64_array.of_array res in
  let res_i64_col =
    Arrow_array.Array_intf.Array
      ((module Arrow_array.Primitive_array.Int64_array), res_array)
  in

  let col = cols.(2) in
  let f64_array =
    Arrow_array.Primitive_array.Float64_array.(as_array col |> to_array)
  in
  let res =
    Array.map
      (function Some i -> Some (Float.mul i i) | None -> None)
      f64_array
  in
  let res_array = Arrow_array.Primitive_array.Float64_array.of_array res in
  let res_f64_col =
    Arrow_array.Array_intf.Array
      ((module Arrow_array.Primitive_array.Float64_array), res_array)
  in

  let res_cols = [| res_i32_col; res_i64_col; res_f64_col |] in
  let org_schema = Arrow_array.Record_batch.schema batch in
  let res_schema =
    Arrow_schema.Schema.{ fields = Array.sub org_schema.fields 0 3 }
  in
  let res_batch =
    Arrow_array.Record_batch.{ schema = res_schema; columns = res_cols }
  in
  let sw =
    Arrow_ipc.Writer.Writer.create Unix.stdout
    |> Arrow_ipc.Writer.StreamWriter.create
  in
  let _ = Arrow_ipc.Writer.StreamWriter.write sw res_batch in
  ()
in

Arrow_ipc.Reader.read_stream write_back_record_batch_to_stdout Unix.stdin
