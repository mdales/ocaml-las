open Util.Operators

type t = {
  user_id : string;
  record_id : int;
  record_length_after_header : int;
  description : string;
  data : string;
}

let v user_id record_id record_length_after_header description data =
  { user_id; record_id; record_length_after_header; description; data }

let read_reserved buf =
  let* value = Util.read_uint16 buf in
  (* Spec says 0 is the only valid reserver, but LAZ files have 0xAABB *)
  match value with
  | 0 -> Ok ()
  | 0xaabb -> Ok ()
  | _ -> Error (Util.Corrupt_reserved value)

let of_buffer buf =
  let* () = read_reserved buf in
  let* user_id = Util.read_string 16 buf in
  let* record_id = Util.read_uint16 buf in
  let* record_length_after_header = Util.read_uint16 buf in
  let* description = Util.read_string 32 buf in
  let* data = Util.read_bytes record_length_after_header buf in
  Ok (v user_id record_id record_length_after_header description data)

let pp_vlr fmt t =
  Format.fprintf fmt
    "{ user_id = \"%s\"; record_id = 0x%x; record_length_after_header = %d; \
     description = \"%s\"; data = %d bytes of data}"
    t.user_id t.record_id t.record_length_after_header t.description
    (String.length t.data)

let user_id t = t.user_id
let record_id t = t.record_id
let description t = t.description
let data t = t.data
