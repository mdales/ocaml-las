type t

val v : string -> int -> int -> string -> string -> t
val of_buffer : Eio.Buf_read.t -> (t, Util.error) result
val pp_vlr : Format.formatter -> t -> unit
