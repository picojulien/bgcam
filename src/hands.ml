(* Reconnaissance de gestes : chaîne MediaPipe « gesture recognizer »
   réimplémentée au-dessus de ses quatre modèles TFLite.

     détection de paume -> zone orientée de la main -> 21 points de repère
     -> plongement -> classifieur de gestes *)

open Ffi

let labels =
  [| "None"; "Closed_Fist"; "Open_Palm"; "Pointing_Up"; "Thumb_Down";
     "Thumb_Up"; "Victory"; "ILoveYou" |]

let det_size = 192
let lm_size = 224
let n_anchors = 2016
let pi = Float.pi

type t = {
  det : Tflite.t;
  lm : Tflite.t;
  emb : Tflite.t;
  cls : Tflite.t;
  anchors : float array; (* centres x, y de chaque ancre, normalisés *)
  det_in : floats;
  boxes : floats; (* n_anchors x 18 : boîte puis 7 points clés *)
  scores : floats;
  lm_in : floats;
  lm_out : floats; (* 21 x (x, y, z) en pixels de la zone *)
  presence : floats;
  handedness : floats;
  world : floats;
  emb_hand : floats;
  emb_world : floats;
  emb_slots : int * int * int; (* indices des entrées hand, handedness, world *)
  embedding : floats;
  probs : floats;
}

(* Ancres SSD du détecteur de paume : pas de 8 (2 ancres par cellule) puis
   trois couches de pas 16 fusionnées (6 ancres par cellule). *)
let make_anchors () =
  let a = Array.make (n_anchors * 2) 0. in
  let k = ref 0 in
  List.iter
    (fun (stride, per_cell) ->
      let n = det_size / stride in
      for y = 0 to n - 1 do
        for x = 0 to n - 1 do
          for _ = 1 to per_cell do
            a.(!k) <- (float x +. 0.5) /. float n;
            a.(!k + 1) <- (float y +. 0.5) /. float n;
            k := !k + 2
          done
        done
      done)
    [ (8, 2); (16, 6) ];
  assert (!k = n_anchors * 2);
  a

let create () =
  let m = Models.hands () in
  let emb = Tflite.load m.embedder 1 in
  let slot name =
    let rec find i = if Tflite.input_name emb i = name then i else find (i + 1) in
    find 0
  in
  {
    det = Tflite.load m.detector 2;
    lm = Tflite.load m.landmarks 2;
    emb;
    cls = Tflite.load m.classifier 1;
    anchors = make_anchors ();
    det_in = floats (det_size * det_size * 3);
    boxes = floats (n_anchors * 18);
    scores = floats n_anchors;
    lm_in = floats (lm_size * lm_size * 3);
    lm_out = floats 63;
    presence = floats 1;
    handedness = floats 1;
    world = floats 63;
    emb_hand = floats 63;
    emb_world = floats 63;
    emb_slots = (slot "hand", slot "handedness", slot "world_hand");
    embedding = floats 128;
    probs = floats (Array.length labels);
  }

let sigmoid x = 1. /. (1. +. exp (-.Float.max (-100.) (Float.min 100. x)))

(* Zone orientée de la main, en pixels de l'image. *)
type roi = { cx : float; cy : float; side : float; rot : float }

(* Détecte la paume la plus probable. L'image est réduite en conservant ses
   proportions, avec des bandes noires. *)
let detect t (frame : Img.t) =
  let scale = float (max frame.w frame.h) /. float det_size in
  let pad_x = (float det_size -. (float frame.w /. scale)) /. 2.
  and pad_y = (float det_size -. (float frame.h /. scale)) /. 2. in
  Img.warp frame t.det_in det_size ~ox:(-.pad_x *. scale) ~oy:(-.pad_y *. scale)
    ~ax:scale ~ay:0. ~bx:0. ~by:scale;
  Tflite.set_input t.det 0 t.det_in;
  Tflite.invoke t.det;
  Tflite.get_output t.det 0 t.boxes;
  Tflite.get_output t.det 1 t.scores;
  let best = ref 0 in
  for i = 1 to n_anchors - 1 do
    if t.scores.{i} > t.scores.{!best} then best := i
  done;
  if sigmoid t.scores.{!best} < 0.5 then None
  else
    (* Coordonnées décodées de l'ancre [i], en pixels du détecteur. *)
    let decode i j =
      let anchor = t.anchors.((i * 2) + (j land 1)) *. float det_size in
      if j = 2 || j = 3 then t.boxes.{(i * 18) + j}
      else t.boxes.{(i * 18) + j} +. anchor
    in
    let iou i j =
      let overlap c s =
        let a = decode i c and b = decode j c in
        let wa = decode i s and wb = decode j s in
        Float.max 0.
          (Float.min (a +. (wa /. 2.)) (b +. (wb /. 2.))
          -. Float.max (a -. (wa /. 2.)) (b -. (wb /. 2.)))
      in
      let inter = overlap 0 2 *. overlap 1 3 in
      inter /. ((decode i 2 *. decode i 3) +. (decode j 2 *. decode j 3) -. inter)
    in
    (* Moyenne pondérée des détections qui recouvrent la meilleure : plus
       stable que la meilleure seule. *)
    let acc = Array.make 18 0. and total = ref 0. in
    for i = 0 to n_anchors - 1 do
      let s = sigmoid t.scores.{i} in
      if s >= 0.5 && iou !best i > 0.3 then (
        total := !total +. s;
        for j = 0 to 17 do
          acc.(j) <- acc.(j) +. (s *. decode i j)
        done)
    done;
    let v j = acc.(j) /. !total in
    let px x = (x -. pad_x) *. scale and py y = (y -. pad_y) *. scale in
    let w = v 2 *. scale and h = v 3 *. scale in
    (* Orientation : du poignet (point 0) vers la base du majeur (point 2),
       ramenée à la verticale. *)
    let wx = px (v 4) and wy = py (v 5) and mx = px (v 8) and my = py (v 9) in
    let rot = (pi /. 2.) -. atan2 (-.(my -. wy)) (mx -. wx) in
    let rot = rot -. (2. *. pi *. Float.floor ((rot +. pi) /. (2. *. pi))) in
    (* La zone est décalée vers les doigts et agrandie pour couvrir toute
       la main. *)
    Some
      {
        cx = px (v 0) +. (0.5 *. h *. sin rot);
        cy = py (v 1) -. (0.5 *. h *. cos rot);
        side = 2.6 *. Float.max w h;
        rot;
      }

(* Divise par l'étendue de la main (en x et y), après avoir pris le poignet
   comme origine. *)
let normalize (m : floats) =
  let ox = m.{0} and oy = m.{1} and oz = m.{2} in
  let lo_x = ref infinity and hi_x = ref neg_infinity in
  let lo_y = ref infinity and hi_y = ref neg_infinity in
  for i = 0 to 20 do
    m.{i * 3} <- m.{i * 3} -. ox;
    m.{(i * 3) + 1} <- m.{(i * 3) + 1} -. oy;
    m.{(i * 3) + 2} <- m.{(i * 3) + 2} -. oz;
    lo_x := Float.min !lo_x m.{i * 3};
    hi_x := Float.max !hi_x m.{i * 3};
    lo_y := Float.min !lo_y m.{(i * 3) + 1};
    hi_y := Float.max !hi_y m.{(i * 3) + 1}
  done;
  let size = Float.max (!hi_x -. !lo_x) (!hi_y -. !lo_y) +. 1e-5 in
  for i = 0 to 62 do
    m.{i} <- m.{i} /. size
  done

(* Calcule le plongement de la main la plus visible dans [t.embedding].
   Faux s'il n'y a pas de main. *)
let embed t (frame : Img.t) =
  match detect t frame with
  | None -> false
  | Some r ->
      let c = cos r.rot and s = sin r.rot in
      let step = r.side /. float lm_size in
      (* Coin haut-gauche de la zone tournée. *)
      let ox = r.cx -. (r.side /. 2. *. (c -. s))
      and oy = r.cy -. (r.side /. 2. *. (s +. c)) in
      Img.warp frame t.lm_in lm_size ~ox ~oy ~ax:(c *. step) ~ay:(s *. step)
        ~bx:(-.s *. step) ~by:(c *. step);
      Tflite.set_input t.lm 0 t.lm_in;
      Tflite.invoke t.lm;
      Tflite.get_output t.lm 0 t.lm_out;
      Tflite.get_output t.lm 1 t.presence;
      Tflite.get_output t.lm 2 t.handedness;
      Tflite.get_output t.lm 3 t.world;
      if t.presence.{0} < 0.5 then false
      else begin
        (* Points de repère ramenés dans l'image, en coordonnées où le plus
           grand côté de l'image vaut 1. *)
        let dim = float (max frame.w frame.h) in
        for i = 0 to 20 do
          let lx = (t.lm_out.{i * 3} /. float lm_size) -. 0.5
          and ly = (t.lm_out.{(i * 3) + 1} /. float lm_size) -. 0.5 in
          t.emb_hand.{i * 3} <- (r.cx +. (((lx *. c) -. (ly *. s)) *. r.side)) /. dim;
          t.emb_hand.{(i * 3) + 1} <-
            (r.cy +. (((lx *. s) +. (ly *. c)) *. r.side)) /. dim;
          t.emb_hand.{(i * 3) + 2} <-
            t.lm_out.{(i * 3) + 2} /. float lm_size *. r.side /. dim;
          let wx = t.world.{i * 3} and wy = t.world.{(i * 3) + 1} in
          t.emb_world.{i * 3} <- (wx *. c) -. (wy *. s);
          t.emb_world.{(i * 3) + 1} <- (wx *. s) +. (wy *. c);
          t.emb_world.{(i * 3) + 2} <- t.world.{(i * 3) + 2}
        done;
        normalize t.emb_hand;
        normalize t.emb_world;
        let hand, handedness, world = t.emb_slots in
        Tflite.set_input t.emb hand t.emb_hand;
        Tflite.set_input t.emb handedness t.handedness;
        Tflite.set_input t.emb world t.emb_world;
        Tflite.invoke t.emb;
        Tflite.get_output t.emb 0 t.embedding;
        true
      end

(* Plongement courant, copié. *)
let embedding t = Array.init 128 (fun i -> t.embedding.{i})

(* Geste prédéfini le plus probable pour le plongement courant. *)
let rec classify t =
  Tflite.set_input t.cls 0 t.embedding;
  Tflite.invoke t.cls;
  Tflite.get_output t.cls 0 t.probs;
  let best = ref 0 in
  for i = 1 to Array.length labels - 1 do
    if t.probs.{i} > t.probs.{!best} then best := i
  done;
  (labels.(!best), t.probs.{!best})

(* Geste prédéfini le plus probable pour un plongement donné. *)
and classify_vec t (v : float array) =
  Array.iteri (fun i x -> t.embedding.{i} <- x) v;
  classify t

(* Geste prédéfini le plus probable et sa confiance, ou None sans main. *)
let recognize t frame = if embed t frame then Some (classify t) else None
