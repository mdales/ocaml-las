open Oclas

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file.las>\n" Sys.argv.(0);
    exit 1);
  let filename = Sys.argv.(1) in

  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Path.with_open_in Eio.Path.(fs / filename) @@ fun file ->
  let buf = Eio.Buf_read.of_flow ~max_size:max_int file in
  match Las.of_buffer buf with
  | Ok las ->
      let header = Las.header las in
      let version_major, version_minor = Header.version header
      and format =
        match Las.is_laz las with
        | true -> "Compressed LAZ file"
        | false -> "Uncompressed LAS file"
      in
      Format.printf "%s, version %d.%d\n" format version_major version_minor;
      let point_count = Header.number_of_point_records header in
      Format.printf "Total points count: %d\n" point_count;
      let (min_x, min_y, min_z), (max_x, max_y, max_z) = Header.bounds header in
      Format.printf "Bounds:\n\tx: %f -> %f\n\ty: %f -> %f\n\tz: %f -> %f\n"
        min_x max_x min_y max_y min_z max_z;
      let proj = Las.projection las in
      Format.printf "CRS:\n\t%s\n" proj
  | Error e ->
      Format.eprintf "Error: %a\n" Util.pp_error e;
      exit 1
