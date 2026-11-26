(* Détourage : masque de la personne (0..255) à la résolution du modèle. *)

open Bigarray
open Ffi

let size = 256

(* Seuils et lissage : voir [Live]. Un pixel entre dans la personne
   au-dessus de [enter] et n'en sort qu'en dessous de [leave] : entre les
   deux, il garde son état, ce qui supprime le clignotement des zones
   incertaines. *)

type t = {
  model : Tflite.t;
  input : floats;
  output : floats;
  channels : int;
  mask : bytes8;
  prob : float array; (* probabilité lissée dans l'espace *)
  inside : Bytes.t; (* état de chaque pixel, avec hystérésis *)
  smooth : float array; (* masque lissé dans le temps *)
  mutable primed : bool;
  mutable coverage : float; (* part de l'image occupée par la personne *)
  live : Live.t;
}

let create (live : Live.t) =
  let model = Tflite.load (Models.segmenter ()) 2 in
  let len = Tflite.output_len model 0 in
  {
    model;
    input = floats (size * size * 3);
    output = floats len;
    channels = len / (size * size);
    mask = bytes8 (size * size);
    prob = Array.make (size * size) 0.;
    inside = Bytes.make (size * size) '\000';
    smooth = Array.make (size * size) 0.;
    primed = false;
    coverage = 0.;
    live;
  }

(* Moyenne 3x3 de [src] dans [dst]. *)
let box3 src dst =
  for y = 0 to size - 1 do
    for x = 0 to size - 1 do
      let acc = ref 0. and n = ref 0 in
      for yy = max 0 (y - 1) to min (size - 1) (y + 1) do
        for xx = max 0 (x - 1) to min (size - 1) (x + 1) do
          acc := !acc +. src ((yy * size) + xx);
          incr n
        done
      done;
      dst ((y * size) + x) (!acc /. float !n)
    done
  done

let mask t (frame : Img.t) =
  let s = float size in
  Img.warp frame t.input size ~ox:0. ~oy:0.
    ~ax:(float frame.w /. s) ~ay:0. ~bx:0. ~by:(float frame.h /. s);
  Tflite.set_input t.model 0 t.input;
  Tflite.invoke t.model;
  Tflite.get_output t.model 0 t.output;
  (* Un seul canal : probabilité de la personne. Sinon le fond vient en
     premier et la personne en dernier. *)
  let ch = t.channels and last = t.channels - 1 in
  let raw i = Array1.unsafe_get t.output ((i * ch) + last) in
  (* Moyenne 3x3 : retire le bruit pixel par pixel. *)
  box3 raw (fun i v -> t.prob.(i) <- v);
  let enter = t.live.enter and leave = t.live.leave and rate = t.live.rate in
  for i = 0 to (size * size) - 1 do
    let p = t.prob.(i) in
    let was = Bytes.unsafe_get t.inside i = '\001' in
    let now = if was then p >= leave else p > enter in
    Bytes.unsafe_set t.inside i (if now then '\001' else '\000');
    let target = if now then 1. else 0. in
    let m = if t.primed then (rate *. target) +. ((1. -. rate) *. t.smooth.(i)) else target in
    t.smooth.(i) <- m
  done;
  (* L'état étant tout ou rien, un dernier flou adoucit les bords. *)
  let inside = ref 0 in
  box3 (Array.get t.smooth) (fun i m ->
      if m > 0.5 then incr inside;
      Array1.unsafe_set t.mask i (int_of_float ((m *. 255.) +. 0.5)));
  t.coverage <- float !inside /. float (size * size);
  t.primed <- true;
  t.mask
