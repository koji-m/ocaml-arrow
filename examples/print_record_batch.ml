let print_i32_record_batch batch =
  let cols = Arrow_array.Record_batch.columns batch in
  Array.iter
    (fun col ->
      let i32_array =
        Arrow_array.Primitive_array.Int32_array.(as_array col |> to_array)
      in
      Printf.printf "i32_array: [\n";
      Array.iter
        (fun i ->
          match i with
          | Some i -> Printf.printf "  %d,\n" (Int32.to_int i)
          | None -> Printf.printf "  null, \n")
        i32_array;
      Printf.printf "]\n")
    cols
in

Arrow_ipc.Reader.read_stream print_i32_record_batch Unix.stdin
