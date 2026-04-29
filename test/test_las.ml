open Oclas

let make_buf s =
  Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:max_int

let ok_header = Alcotest.(result (of_pp Las.pp_header) (of_pp Las.pp_error))

let test_valid_header () =
  let result =
    Las.of_buffer
      (make_buf
         "LASF\x20\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x04")
  in
  let expected = Las.v 32 7 (1, 4) in
  Alcotest.(check ok_header) "valid magic" (Ok expected) result

let test_unsupported_major_version () =
  let result =
    Las.of_buffer
      (make_buf
         "LASF\x20\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x04")
  in
  Alcotest.(check ok_header) "wrong major" (Error (Unsupported_version (2, 4))) result

let test_unsupported_minor_version () =
  let result =
    Las.of_buffer
      (make_buf
         "LASF\x20\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x08")
  in
  Alcotest.(check ok_header) "wrong major" (Error (Unsupported_version (1, 8))) result

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
          Alcotest.test_case "wrong major version" `Quick test_unsupported_major_version;
          Alcotest.test_case "wrong minor version" `Quick test_unsupported_minor_version;
          Alcotest.test_case "wrong magic bytes" `Quick test_wrong_magic;
          Alcotest.test_case "truncated input" `Quick test_truncated_input;
          Alcotest.test_case "empty input" `Quick test_empty_input;
        ] );
    ]
