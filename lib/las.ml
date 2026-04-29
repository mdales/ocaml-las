(* type t = bool *)

let las_magic = "LASF"

let of_buffer buf =
    match Eio.Buf_read.take 4 buf with
    | s -> (match (String.equal s las_magic) with true -> Result.Ok true | false -> Result.Error "Wrong header magic")
    | exception End_of_file -> Result.Error "Too few bytes"
