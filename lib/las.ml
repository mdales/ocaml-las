open Util.Operators

type t = { header : Header.t; vlrs : Vlr.t list }

let of_buffer buf =
  let* header = Header.of_buffer buf in
  let vlr_count = Header.variable_length_record_count header in

  let rec loop count acc =
    match count with
    | 0 -> Ok acc
    | n -> (
        let vlr_r = Vlr.of_buffer buf in
        match vlr_r with Error e -> Error e | Ok v -> loop (n - 1) (v :: acc))
  in

  let* vlrs = loop vlr_count [] in
  Ok { header; vlrs }

let header t = t.header
let vlrs t = t.vlrs
