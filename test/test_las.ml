let make_buf s =
  Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:max_int

let ok_bool = Alcotest.(result bool string)

let test_valid_magic () =
    let result = Oclas.Las.of_buffer (make_buf "LASF\x00\x00\x00\x00") in
    Alcotest.(check ok_bool) "valid magic" (Ok true) result

let test_wrong_magic () =
    let result = Oclas.Las.of_buffer (make_buf "XXXX\x00\x00\x00\x00") in
    Alcotest.(check ok_bool) "wrong magic" (Error "Wrong header magic") result

let test_truncated_input () =
    (* Fewer than 4 bytes — the reader should return false, not raise *)
    let result = Oclas.Las.of_buffer (make_buf "LAS") in
    Alcotest.(check ok_bool) "truncated input" (Error "Too few bytes") result

let test_empty_input () =
    let result = Oclas.Las.of_buffer (make_buf "") in
    Alcotest.(check ok_bool) "empty input" (Error "Too few bytes") result

let () =
    Alcotest.run "Las_magic" [
        "magic", [
            Alcotest.test_case "valid magic bytes"   `Quick test_valid_magic;
            Alcotest.test_case "wrong magic bytes"   `Quick test_wrong_magic;
            Alcotest.test_case "truncated input"     `Quick test_truncated_input;
            Alcotest.test_case "empty input"         `Quick test_empty_input;
        ]
    ]
