type encoding =
  | GPS_time_type
  | Waveform_data_packets_internal
  | Waveform_data_packets_external
  | Synthetic_return_numbers
  | WKT
  | Unknown of int (* bit position *)

type t

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
  t

val of_buffer : Eio.Buf_read.t -> (t, Util.error) result
val global_encoding : t -> encoding list
val version : t -> int * int
val system_identifier : t -> string
val generating_software : t -> string
val variable_length_record_count : t -> int
val pp_encoding : Format.formatter -> encoding -> unit
val pp_header : Format.formatter -> t -> unit
