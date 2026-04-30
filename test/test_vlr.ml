open Oclas
open Utils

let test_vlr =
  make_buf
    (String.concat ""
       [
         bytes_of_uint16_le 0;
         (* reserved *)
         padded_string "user_id" 16;
         (* user_id *)
         bytes_of_uint16_le 0x0402;
         (* record_id *)
         bytes_of_uint16_le 0x0008;
         (* record_length_after_header *)
         padded_string "description" 32;
         padded_string "hello123" 8;
       ])

let ok_vlr = Alcotest.(result (of_pp Vlr.pp_vlr) (of_pp Util.pp_error))

let test_valid_vlr () =
  let result = Vlr.of_buffer test_vlr in
  let expected = Vlr.v "user_id" 0x0402 8 "description" "hello123" in
  Alcotest.(check ok_vlr) "valid vlr" (Ok expected) result

let () =
  Alcotest.run "LAS VLR"
    [ ("vlr", [ Alcotest.test_case "valid vlr" `Quick test_valid_vlr ]) ]
