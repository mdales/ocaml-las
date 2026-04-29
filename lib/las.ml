type error =
  | Invalid_file_signature
  | Unsupported_version of int * int
  | Truncated of string (* field name *)

type t = {
  file_source_id : int;
  global_encoding : int;
  version : int * int;
  system_identifier : string;
  generating_software : string;
  header_size : int;
  offset_to_point_data : int;
  vlr_count : int;
}

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
  | exception End_of_file -> Error (Truncated "uint16")

let read_uint32 buf =
  match Eio.Buf_read.LE.uint32 buf with
  | v -> Ok (Int32.to_int v land 0xFFFFFFFF)
  | exception End_of_file -> Error (Truncated "uint32")

let skip_bytes n buf =
  match Eio.Buf_read.skip n buf with
  | () -> Ok ()
  | exception End_of_file -> Error (Truncated "skip")

let read_version buf =
  try
    let major = Eio.Buf_read.uint8 buf in
    let minor = Eio.Buf_read.uint8 buf in
    if major <> 1 || minor > 5 then Error (Unsupported_version (major, minor))
    else Ok (major, minor)
  with End_of_file -> Error (Truncated "version")

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

(* Public *)

let v file_source_id global_encoding version system_identifier
    generating_software header_size offset_to_point_data vlr_count =
  {
    file_source_id;
    global_encoding;
    version;
    system_identifier;
    generating_software;
    header_size;
    offset_to_point_data;
    vlr_count;
  }

let of_buffer buf =
  let* () = read_magic buf in
  let* file_source_id = read_uint16 buf in
  let* global_encoding = read_uint16 buf in
  let* () = skip_bytes 16 buf in
  (* GUID *)
  let* version = read_version buf in
  let* system_identifier = read_string 32 buf in
  let* generating_software = read_string 32 buf in
  let* () = skip_bytes 4 buf in
  (* dates *)
  let* header_size = read_uint16 buf in
  let* offset_to_point_data = read_uint32 buf in
  let* vlr_count = read_uint32 buf in
  Result.Ok
    (v file_source_id global_encoding version system_identifier
       generating_software header_size offset_to_point_data vlr_count)

let version t = t.version
let system_identifier t = t.system_identifier
let generating_software t = t.generating_software

let pp_header fmt t =
  let version_major, version_minor = t.version in
  Format.fprintf fmt
    "{ file_source_id = 0x%x; global_encoding = 0x%0x; version = %d.%d; \
     system_identifier = \"%s\"; generating_software = \"%s\"; header_size = \
     0x%x; offset_to_point_data = 0x%x; vlr_count = %d }"
    t.file_source_id t.global_encoding version_major version_minor
    t.system_identifier t.generating_software t.header_size
    t.offset_to_point_data t.vlr_count

let pp_error fmt = function
  | Invalid_file_signature -> Format.fprintf fmt "Invalid_file_signature"
  | Unsupported_version (maj, min) ->
      Format.fprintf fmt "Unsupported_version (%d, %d)" maj min
  | Truncated field -> Format.fprintf fmt "Truncated %S" field
