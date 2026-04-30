type t
(** LAS Variable Length Record *)

val v : string -> int -> int -> string -> string -> t
(** [v user_id record_id record_length_after_header description data] *)

val of_buffer : Eio.Buf_read.t -> (t, Util.error) result
(** [of_buffer buf] Reads a VLR from the head of an EIO read buffer. Will fail
    if the reserved field isn't one of the valid values for LAS and LAZ files.
*)

val pp_vlr : Format.formatter -> t -> unit
(** pretty printer *)

val user_id : t -> string
(** [user_id vlr] Returns the user_id string of a VLR. *)

val record_id : t -> int
(** [record_id vlr] Returns the 16 bit record ID of the VLR. *)

val description : t -> string
(** [decription vlr] Returns the decription string of the VLR. *)

val data : t -> string
(** [data vlr] Returns the raw data from the VLR. *)
