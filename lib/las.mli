type t

val of_buffer : Eio.Buf_read.t -> (t, Util.error) result
val header : t -> Header.t
val vlrs : t -> Vlr.t list
