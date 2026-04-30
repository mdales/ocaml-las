open Oclas

let bytes_of_uint8 n = String.make 1 (Char.chr n)

let bytes_of_uint16_le n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_le b 0 n;
  Bytes.to_string b

let bytes_of_uint32_le n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.to_string b

let bytes_of_double_le f =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.bits_of_float f);
  Bytes.to_string b

let padded_string s n =
  let l = String.length s in
  match Int.compare l n with
  | 0 -> s
  | c when c < 0 -> s ^ String.make (n - l) '\x00'
  | _ -> failwith "String too long"

let make_buf s =
  Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:max_int

let test_header =
  make_buf
    (String.concat ""
       [
         "LASF";
         (* magic *)
         bytes_of_uint16_le 32;
         (* file source id *)
         bytes_of_uint16_le 0x11;
         (* global encoding *)
         String.make 16 '\x00';
         (* GUID - currently ignored *)
         bytes_of_uint8 1;
         (* version major *)
         bytes_of_uint8 4;
         (* version minor *)
         padded_string "system" 32;
         (* system identifier *)
         padded_string "software" 32;
         (* generating software *)
         bytes_of_uint16_le 0;
         (* day of year *)
         bytes_of_uint16_le 0;
         (* year *)
         bytes_of_uint16_le 42;
         (* header length *)
         bytes_of_uint32_le 123;
         (* offset of point data *)
         bytes_of_uint32_le 234;
         (* number of VLR records *)
         bytes_of_uint8 0;
         (* point data record format *)
         bytes_of_uint16_le 10;
         String.make 24 '\x00';
         (* legacy point counts *)
         (* point data record length *)
         bytes_of_double_le 1.0;
         bytes_of_double_le 2.0;
         bytes_of_double_le 3.0;
         (* scale *)
         bytes_of_double_le 11.0;
         bytes_of_double_le 12.0;
         bytes_of_double_le 13.0;
         (* offset *)
       ])

let ok_header = Alcotest.(result (of_pp Las.pp_header) (of_pp Las.pp_error))

let test_valid_header () =
  let result = Las.of_buffer test_header in
  let expected =
    Las.v 32
      [ Las.GPS_time_type; Las.WKT ]
      (1, 4) "system" "software" 42 123 234 0 10 (1., 2., 3.) (11., 12., 13.)
  in
  Alcotest.(check ok_header) "valid magic" (Ok expected) result

let test_unsupported_major_version () =
  let result =
    Las.of_buffer
      (make_buf
         "LASF\x20\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x04")
  in
  Alcotest.(check ok_header)
    "wrong major"
    (Error (Unsupported_version (2, 4)))
    result

let test_unsupported_minor_version () =
  let result =
    Las.of_buffer
      (make_buf
         "LASF\x20\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x08")
  in
  Alcotest.(check ok_header)
    "wrong major"
    (Error (Unsupported_version (1, 8)))
    result

let test_wrong_magic () =
  let result = Las.of_buffer (make_buf "XXXX\x00\x00\x00\x00") in
  Alcotest.(check ok_header)
    "wrong magic" (Error Las.Invalid_file_signature) result

let test_truncated_input () =
  (* Fewer than 4 bytes — the reader should return false, not raise *)
  let result = Las.of_buffer (make_buf "LAS") in
  Alcotest.(check ok_header)
    "truncated input" (Error (Las.Truncated "File Signature")) result

let test_empty_input () =
  let result = Las.of_buffer (make_buf "") in
  Alcotest.(check ok_header)
    "truncated input" (Error (Las.Truncated "File Signature")) result

let () =
  Alcotest.run "Las_magic"
    [
      ( "magic",
        [
          Alcotest.test_case "valid magic bytes" `Quick test_valid_header;
          Alcotest.test_case "wrong major version" `Quick
            test_unsupported_major_version;
          Alcotest.test_case "wrong minor version" `Quick
            test_unsupported_minor_version;
          Alcotest.test_case "wrong magic bytes" `Quick test_wrong_magic;
          Alcotest.test_case "truncated input" `Quick test_truncated_input;
          Alcotest.test_case "empty input" `Quick test_empty_input;
        ] );
    ]
