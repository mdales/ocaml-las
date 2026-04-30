open Util.Operators

type encoding =
  | GPS_time_type
  | Waveform_data_packets_internal
  | Waveform_data_packets_external
  | Synthetic_return_numbers
  | WKT
  | Unknown of int (* bit position *)

type t = {
  file_source_id : int;
  global_encoding : encoding list;
  version : int * int;
  system_identifier : string;
  generating_software : string;
  header_size : int;
  offset_to_point_data : int;
  variable_length_record_count : int;
  point_data_record_format : int;
  point_data_record_length : int;
  scale : float * float * float;
  offset : float * float * float;
  min : float * float * float;
  max : float * float * float;
  start_of_waveform_data_packet_record : int;
  start_of_first_extended_variable_length_record : int;
  number_of_extended_variable_length_records : int;
  number_of_point_records : int;
  number_of_points_by_return : int list;
}

let las_magic = "LASF"


(* Private helpers *)

let read_magic buf =
  match Eio.Buf_read.take 4 buf with
  | s -> (
      match String.equal s las_magic with
      | true -> Ok ()
      | false -> Error Util.Invalid_file_signature)
  | exception End_of_file -> Error (Truncated "File Signature")

let read_double_triple buf =
  let* x = Util.read_double buf in
  let* y = Util.read_double buf in
  let* z = Util.read_double buf in
  Ok (x, y, z)

let read_global_encoding buf =
  let* encoding = Util.read_uint16 buf in
  let rencodings =
    List.init 16 (fun idx ->
        let bit = (encoding lsr idx) land 0x01 in
        if bit = 1 then
          let e =
            match idx with
            | 0 -> GPS_time_type
            | 1 -> Waveform_data_packets_internal
            | 2 -> Waveform_data_packets_external
            | 3 -> Synthetic_return_numbers
            | 4 -> WKT
            | x -> Unknown x
          in
          Some e
        else None)
  in
  let encodings = List.filter_map (fun x -> x) rencodings in
  Ok encodings

let read_version buf =
  try
    let major = Eio.Buf_read.uint8 buf in
    let minor = Eio.Buf_read.uint8 buf in
    if major <> 1 || minor > 5 then Error (Util.Unsupported_version (major, minor))
    else Ok (major, minor)
  with End_of_file -> Error (Util.Truncated "version")


(* Public *)

let v file_source_id global_encoding version system_identifier
    generating_software header_size offset_to_point_data
    variable_length_record_count point_data_record_format
    point_data_record_length scale offset min max
    start_of_waveform_data_packet_record
    start_of_first_extended_variable_length_record
    number_of_extended_variable_length_records number_of_point_records
    number_of_points_by_return =
  {
    file_source_id;
    global_encoding;
    version;
    system_identifier;
    generating_software;
    header_size;
    offset_to_point_data;
    variable_length_record_count;
    point_data_record_format;
    point_data_record_length;
    scale;
    offset;
    min;
    max;
    start_of_waveform_data_packet_record;
    start_of_first_extended_variable_length_record;
    number_of_extended_variable_length_records;
    number_of_point_records;
    number_of_points_by_return;
  }

let of_buffer buf =
  let* () = read_magic buf in
  let* file_source_id = Util.read_uint16 buf in
  let* global_encoding = read_global_encoding buf in
  let* () = Util.skip_bytes 16 buf in
  (* GUID *)
  let* version = read_version buf in
  let* system_identifier = Util.read_string 32 buf in
  let* generating_software = Util.read_string 32 buf in
  let* () = Util.skip_bytes 4 buf in
  (* dates *)
  let* header_size = Util.read_uint16 buf in
  let* offset_to_point_data = Util.read_uint32 buf in
  let* vlr_count = Util.read_uint32 buf in
  let* point_data_record_format = Util.read_byte buf in
  let* point_data_record_length = Util.read_uint16 buf in
  let* () = Util.skip_bytes 4 buf in
  (* Legacy number of point records *)
  let* () = Util.skip_bytes 20 buf in
  (* Legacy number of point by returns *)
  let* scale = read_double_triple buf in
  let* offset = read_double_triple buf in
  let* max_x = Util.read_double buf in
  let* min_x = Util.read_double buf in
  let* max_y = Util.read_double buf in
  let* min_y = Util.read_double buf in
  let* max_z = Util.read_double buf in
  let* min_z = Util.read_double buf in
  let* start_of_waveform_data_packet_record = Util.read_uint64 buf in
  let* start_of_first_extended_variable_length_record = Util.read_uint64 buf in
  let* number_of_extended_variable_length_records = Util.read_uint32 buf in
  let* number_of_point_records = Util.read_uint64 buf in
  let* number_of_points_by_return = Util.read_uint64_list buf 15 in
  Result.Ok
    (v file_source_id global_encoding version system_identifier
       generating_software header_size offset_to_point_data vlr_count
       point_data_record_format point_data_record_length scale offset
       (min_x, min_y, min_z) (max_x, max_y, max_z)
       start_of_waveform_data_packet_record
       start_of_first_extended_variable_length_record
       number_of_extended_variable_length_records number_of_point_records
       number_of_points_by_return)

let global_encoding t = t.global_encoding
let version t = t.version
let system_identifier t = t.system_identifier
let generating_software t = t.generating_software

let pp_encoding fmt = function
  | GPS_time_type -> Format.fprintf fmt "GPS_time_type"
  | Waveform_data_packets_internal ->
      Format.fprintf fmt "Waveform_data_packets_internal"
  | Waveform_data_packets_external ->
      Format.fprintf fmt "Waveform_data_packets_external"
  | Synthetic_return_numbers -> Format.fprintf fmt "Synthetic_return_numbers"
  | WKT -> Format.fprintf fmt "WKT"
  | Unknown bit -> Format.fprintf fmt "Unknown %d" bit

let pp_header fmt t =
  let version_major, version_minor = t.version in
  let scale_x, scale_y, scale_z = t.scale in
  let offset_x, offset_y, offset_z = t.offset in
  let min_x, min_y, min_z = t.min in
  let max_x, max_y, max_z = t.max in
  Format.fprintf fmt
    "{ file_source_id = 0x%x; global_encoding = [%a]; version = %d.%d; \
     system_identifier = \"%s\"; generating_software = \"%s\"; header_size = \
     0x%x; offset_to_point_data = 0x%x; vlr_count = %d; \
     point_data_record_format = 0x%x; point_data_record_length = %d; scale = \
     (%f, %f, %f); offset = (%f, %f, %f); min = (%f, %f, %f); max = (%f, %f, \
     %f); start_of_waveform_data_packet_record = 0x%x; \
     start_of_first_extended_variable_length_record = 0x%x;\n\
    \      number_of_extended_variable_length_records = %d; \
     number_of_point_records = %d; number_of_points_by_return = [%a]}"
    t.file_source_id
    (Format.pp_print_list pp_encoding)
    t.global_encoding version_major version_minor t.system_identifier
    t.generating_software t.header_size t.offset_to_point_data
    t.variable_length_record_count t.point_data_record_format
    t.point_data_record_length scale_x scale_y scale_z offset_x offset_y
    offset_z min_x min_y min_z max_x max_y max_z
    t.start_of_waveform_data_packet_record
    t.start_of_first_extended_variable_length_record
    t.number_of_extended_variable_length_records t.number_of_point_records
    (Format.pp_print_list Format.pp_print_int)
    t.number_of_points_by_return
