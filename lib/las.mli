type t
(** LAS file data *)

val of_buffer : Eio.Buf_read.t -> (t, Util.error) result

val header : t -> Header.t
(** [header t] Returns the public header of the LAS file. *)

val vlrs : t -> Vlr.t list
(** [vlrs t] Returns a list of the Variable Length Records of the LAS file. *)

val projection : t -> string
(** [projection t] Returns the WKT of the CRS used for the point data. *)
