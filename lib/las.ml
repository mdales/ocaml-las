type error =
  | Invalid_file_signature
  | Unsupported_version of int * int
  | Truncated of string (* field name *)

type t = { file_source_id : int; version : int * int }

let las_magic = "LASF"
let ( let* ) = Result.bind

let read_magic buf =
  match Eio.Buf_read.take 4 buf with
  | s -> (
      match String.equal s las_magic with
      | true -> Result.Ok ()
      | false -> Result.Error Invalid_file_signature)
  | exception End_of_file -> Result.Error (Truncated "File Signature")

let read_uint16 buf =
  match Eio.Buf_read.take 2 buf with
  | s -> Ok (String.get_uint16_le s 0)
  | exception End_of_file -> Result.Error (Truncated "File Source ID")

let v file_source_id version = { file_source_id; version }

let of_buffer buf =
  let* () = read_magic buf in
  let* file_source_id = read_uint16 buf in
  Result.Ok { file_source_id; version = (0, 0) }

let pp_header fmt t =
  let version_major, version_minor = t.version in
  Format.fprintf fmt "{ file_source_id = 0x%x; version = %d.%d; }"
    t.file_source_id version_major version_minor

let pp_error fmt = function
  | Invalid_file_signature -> Format.fprintf fmt "Invalid_file_signature"
  | Unsupported_version (maj, min) ->
      Format.fprintf fmt "Unsupported_version (%d, %d)" maj min
  | Truncated field -> Format.fprintf fmt "Truncated %S" field
