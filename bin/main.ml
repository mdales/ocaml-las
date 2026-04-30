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
      Format.printf "%a\n" Header.pp_header header;
      let vlrs = Las.vlrs las in
      List.iter (fun vlr -> Format.printf "%a\n" Vlr.pp_vlr vlr) vlrs
  | Error e ->
      Format.eprintf "Error: %a\n" Util.pp_error e;
      exit 1
