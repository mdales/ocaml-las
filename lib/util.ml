type error =
  | Invalid_file_signature
  | Corrupt_reserved of int (* corrupt value *)
  | Unsupported_version of int * int
  | Truncated of string (* field name *)

let pp_error fmt = function
  | Invalid_file_signature -> Format.fprintf fmt "Invalid_file_signature"
  | Corrupt_reserved value -> Format.fprintf fmt "Corrupt_reserved: 0x%x" value
  | Unsupported_version (maj, min) ->
      Format.fprintf fmt "Unsupported_version (%d, %d)" maj min
  | Truncated field -> Format.fprintf fmt "Truncated %S" field

module Operators = struct
  let ( let* ) = Result.bind
end

let read_byte buf =
  try Ok (Eio.Buf_read.uint8 buf) with End_of_file -> Error (Truncated "byte")

let read_uint16 buf =
  match Eio.Buf_read.take 2 buf with
  | s -> Ok (String.get_uint16_le s 0)
  | exception End_of_file -> Error (Truncated "uint16")

let read_uint32 buf =
  match Eio.Buf_read.LE.uint32 buf with
  | v -> Ok (Int32.to_int v land 0xFFFFFFFF)
  | exception End_of_file -> Error (Truncated "uint32")

let read_uint64 buf =
  try
    let s = Eio.Buf_read.take 8 buf in
    Ok (Int64.to_int (String.get_int64_le s 0))
  with End_of_file -> Error (Truncated "uint64")

let read_uint64_list buf n =
  try
    Ok
      (List.init n (fun _ ->
           let s = Eio.Buf_read.take 8 buf in
           Int64.to_int (String.get_int64_le s 0)))
  with End_of_file -> Error (Truncated "uint64 list")

let read_double buf =
  try
    let s = Eio.Buf_read.take 8 buf in
    Ok (Int64.float_of_bits (String.get_int64_le s 0))
  with End_of_file -> Error (Truncated "double")

let skip_bytes n buf =
  match Eio.Buf_read.skip n buf with
  | () -> Ok ()
  | exception End_of_file -> Error (Truncated "skip")

let read_string n buf =
  try
    let s = Eio.Buf_read.take n buf in
    (* Find first null byte; if none, use full length *)
    let len =
      match String.index_opt s '\x00' with
      | Some i -> i
      | None -> String.length s
    in
    Ok (String.sub s 0 len)
  with End_of_file -> Error (Truncated "string")

let read_bytes n buf =
  try Ok (Eio.Buf_read.take n buf)
  with End_of_file -> Error (Truncated "bytes")
