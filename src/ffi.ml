(* Liaisons vers stubs.c. *)

open Bigarray

type bytes8 = (int, int8_unsigned_elt, c_layout) Array1.t
type floats = (float, float32_elt, c_layout) Array1.t

let bytes8 n : bytes8 = Array1.create int8_unsigned c_layout n
let floats n : floats = Array1.create float32 c_layout n

module Tflite = struct
  type t

  external load : string -> int -> t = "bgcam_tfl_load"
  external output_len : t -> int -> int = "bgcam_tfl_output_len"
  external input_name : t -> int -> string = "bgcam_tfl_input_name"
  external set_input : t -> int -> floats -> unit = "bgcam_tfl_set_input"
  external get_output : t -> int -> floats -> unit = "bgcam_tfl_get_output"
  external invoke : t -> unit = "bgcam_tfl_invoke"
end

module Jpeg = struct
  external size : string -> int * int = "bgcam_jpeg_size"
  external decode : string -> bytes8 -> int -> int -> unit = "bgcam_jpeg_decode"
  external write : string -> bytes8 -> int -> int -> unit = "bgcam_jpeg_write"
end

module Webp = struct
  type t

  external open_ : string -> t * int * int = "bgcam_webp_open"
  external next : t -> bytes8 -> int = "bgcam_webp_next"
end

module Cam = struct
  type t

  external open_ : string -> int -> int -> int -> t * int * int
    = "bgcam_cam_open"

  external read : t -> bytes8 -> bool = "bgcam_cam_read"
end

module Out = struct
  type t

  external open_ : string -> int -> int -> int -> t = "bgcam_out_open"
  external write : t -> bytes8 -> unit = "bgcam_out_write"
end
