let bytes_of_uint8 n = String.make 1 (Char.chr n)

let bytes_of_uint16_le n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_le b 0 n;
  Bytes.to_string b

let bytes_of_uint32_le n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.to_string b

let bytes_of_uint64_le n =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int n);
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
