let print_record_batch batch =
  let cols = Arrow_array.Record_batch.columns batch in

  let col = cols.(0) in
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
  Printf.printf "]\n";

  let col = cols.(1) in
  let i64_array =
    Arrow_array.Primitive_array.Int64_array.(as_array col |> to_array)
  in
  Printf.printf "i64_array: [\n";
  Array.iter
    (fun i ->
      match i with
      | Some i -> Printf.printf "  %d,\n" (Int64.to_int i)
      | None -> Printf.printf "  null, \n")
    i64_array;
  Printf.printf "]\n";

  let col = cols.(2) in
  let f64_array =
    Arrow_array.Primitive_array.Float64_array.(as_array col |> to_array)
  in
  Printf.printf "f64_array: [\n";
  Array.iter
    (fun f ->
      match f with
      | Some f -> Printf.printf "  %f,\n" f
      | None -> Printf.printf "  null, \n")
    f64_array;
  Printf.printf "]\n"
in

Arrow_ipc.Reader.read_stream print_record_batch Unix.stdin
