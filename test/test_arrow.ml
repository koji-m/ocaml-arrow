open Alcotest
open Arrow_buffer

let test_bit_sub () =
  let len_in_bytes = 2 in
  let len_in_bits = 10 in
  let offset_in_bits = 3 in

  let buf = Buffer.create ~init:'\x00' len_in_bytes in
  let byte_1 = 255 - ((1 lsl offset_in_bits) - 1) in
  let byte_2 = (1 lsl (offset_in_bits + len_in_bits - 8)) - 1 in
  Buffer.set buf 0 (Char.chr byte_1);
  let () = Buffer.set buf 1 (Char.chr byte_2) in

  let buf_expect = Buffer.create ~init:'\x00' 2 in
  let byte_1 = 255 in
  let byte_2 = (1 lsl (len_in_bits - 8)) - 1 in
  Buffer.set buf_expect 0 (Char.chr byte_1);
  let () = Buffer.set buf_expect 1 (Char.chr byte_2) in

  let buf_test = Bit_util.bit_sub buf offset_in_bits len_in_bits in

  Alcotest.(check char)
    "same char 0" (Buffer.get buf_expect 0) (Buffer.get buf_test 0);

  Alcotest.(check char)
    "same char 1" (Buffer.get buf_expect 1) (Buffer.get buf_test 1)

let () =
  run "Arrow_buffer"
    [ ("basic tests", [ test_case "bit_sub works" `Quick test_bit_sub ]) ]
