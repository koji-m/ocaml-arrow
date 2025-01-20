type t = Scalar_buffer.t

let from_length a =
    let byte_width = 4 in
    let b = Buffer.create ((Array.length a + 1) * byte_width) in
    let () = Buffer.set_int32 b 0 0l in
    let _, _ = Array.fold_left
        (fun (i, sum) l -> Buffer.set_int32 b (i * byte_width) (Int32.of_int (sum + l)); (i + 1, sum + l))
        (1, 0)
        a
    in
    Scalar_buffer.({buffer = b; native_type = Native_type.Int32})

let as_buffer b = Scalar_buffer.(b.buffer)

let length a = Scalar_buffer.length a