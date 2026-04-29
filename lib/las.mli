type error =
  | Invalid_file_signature
  | Unsupported_version of int * int
  | Truncated of string (* field name *)

type t

val v : int -> int -> int * int -> string -> string -> int -> int -> int -> t
val of_buffer : Eio.Buf_read.t -> (t, error) result
val version : t -> int * int
val system_identifier : t -> string
val generating_software : t -> string
val pp_error : Format.formatter -> error -> unit
val pp_header : Format.formatter -> t -> unit
