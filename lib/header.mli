type encoding =
  | GPS_time_type
  | Waveform_data_packets_internal
  | Waveform_data_packets_external
  | Synthetic_return_numbers
  | WKT
  | Time_offset_flag
  | Unknown of int (* bit position *)

type t
(** LAS public header record *)

val v :
  int ->
  encoding list ->
  int * int ->
  string ->
  string ->
  int ->
  int ->
  int ->
  int ->
  int ->
  float * float * float ->
  float * float * float ->
  float * float * float ->
  float * float * float ->
  int ->
  int ->
  int ->
  int ->
  int list ->
  float ->
  float ->
  int ->
  t

val of_buffer : Eio.Buf_read.t -> (t, Util.error) result

val global_encoding : t -> encoding list
(** [global_encoding t] Returns a list of the encoding options used in the data.
*)

val version : t -> int * int
(** [version t] Returns the version number of the LAS format used. *)

val system_identifier : t -> string
val generating_software : t -> string

val variable_length_record_count : t -> int
(** [variable_length_record_count t] Returns the number of VLR records in this
    LAS file. *)

val point_data_record_format : t -> int
(** [point_data_record_format t] Returns the version number of the point data
    record format. *)

val bounds : t -> (float * float * float) * (float * float * float)
(** [bounds t] Returns the minimum and maximum bounds of the point cloud. *)

val number_of_point_records : t -> int
(** [number_of_point_records t] Returns the number of point records in the file. *)

val number_of_points_by_return : t -> int list
(** [number_of_points_by_return t] Returns the number of points grouped by return. *)

val pp_encoding : Format.formatter -> encoding -> unit
val pp_header : Format.formatter -> t -> unit
