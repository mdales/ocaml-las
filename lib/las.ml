type error =
  | Invalid_file_signature
  | Unsupported_version of int * int
  | Truncated of string (* field name *)

type t = { file_source_id : int; global_encoding : int; version : int * int }

let las_magic = "LASF"
let ( let* ) = Result.bind

(* Private helpers *)

let read_magic buf =
  match Eio.Buf_read.take 4 buf with
  | s -> (
      match String.equal s las_magic with
      | true -> Ok ()
      | false -> Error Invalid_file_signature)
  | exception End_of_file -> Error (Truncated "File Signature")

let read_uint16 buf =
  match Eio.Buf_read.take 2 buf with
  | s -> Ok (String.get_uint16_le s 0)
  | exception End_of_file -> Error (Truncated "File Source ID")

let skip_bytes n buf =
  match Eio.Buf_read.skip n buf with
  | () -> Ok ()
  | exception End_of_file -> Error (Truncated "skip")

let read_version buf =
  try
    let major = Eio.Buf_read.uint8 buf in
    let minor = Eio.Buf_read.uint8 buf in
    if major <> 1 || minor > 5 then
      Error (Unsupported_version (major, minor))
    else
      Ok (major, minor)
  with End_of_file -> Error (Truncated "version")

(* Public *)

let v file_source_id global_encoding version =
  { file_source_id; global_encoding; version }

let of_buffer buf =
  let* () = read_magic buf in
  let* file_source_id = read_uint16 buf in
  let* global_encoding = read_uint16 buf in
  let* () = skip_bytes 16 buf in
  let* version = read_version buf in
  Result.Ok (v file_source_id global_encoding version)

let pp_header fmt t =
  let version_major, version_minor = t.version in
  Format.fprintf fmt
    "{ file_source_id = 0x%x; global_encoding = 0x%0x; version = %d.%d; }"
    t.file_source_id t.global_encoding version_major version_minor

let pp_error fmt = function
  | Invalid_file_signature -> Format.fprintf fmt "Invalid_file_signature"
  | Unsupported_version (maj, min) ->
      Format.fprintf fmt "Unsupported_version (%d, %d)" maj min
  | Truncated field -> Format.fprintf fmt "Truncated %S" field
