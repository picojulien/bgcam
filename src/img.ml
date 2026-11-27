(* Images RGB24 et opérations par pixel. *)

open Bigarray

type t = { w : int; h : int; d : Ffi.bytes8 }

(* max et min sur les entiers : ceux de Stdlib sont polymorphes et passent
   par une comparaison générique, coûteuse dans les boucles sur les pixels. *)
let max (a : int) b = if a > b then a else b
let min (a : int) b = if a < b then a else b

let create w h = { w; h; d = Ffi.bytes8 (w * h * 3) }
let get (d : Ffi.bytes8) i = Array1.unsafe_get d i
let set (d : Ffi.bytes8) i v = Array1.unsafe_set d i v

(* ------------------------------------------------------------------------
   Groupe de domaines : découpe un intervalle de lignes en tranches
   traitées en parallèle. *)

module Pool = struct
  type t = {
    n : int;
    m : Mutex.t;
    start : Condition.t;
    finished : Condition.t;
    mutable job : int -> unit;
    mutable gen : int;
    mutable pending : int;
  }

  let worker t i =
    let seen = ref 0 in
    while true do
      Mutex.lock t.m;
      while t.gen = !seen do
        Condition.wait t.start t.m
      done;
      seen := t.gen;
      let job = t.job in
      Mutex.unlock t.m;
      job i;
      Mutex.lock t.m;
      t.pending <- t.pending - 1;
      if t.pending = 0 then Condition.signal t.finished;
      Mutex.unlock t.m
    done

  let create n =
    let t =
      {
        n;
        m = Mutex.create ();
        start = Condition.create ();
        finished = Condition.create ();
        job = ignore;
        gen = 0;
        pending = 0;
      }
    in
    for i = 1 to n - 1 do
      ignore (Domain.spawn (fun () -> worker t i))
    done;
    t

  (* Appelle [f lo hi] sur [n] tranches couvrant [0, rows). *)
  let rows t rows f =
    let job i = f (i * rows / t.n) ((i + 1) * rows / t.n) in
    Mutex.lock t.m;
    t.job <- job;
    t.gen <- t.gen + 1;
    t.pending <- t.n - 1;
    Condition.broadcast t.start;
    Mutex.unlock t.m;
    job 0;
    Mutex.lock t.m;
    while t.pending > 0 do
      Condition.wait t.finished t.m
    done;
    Mutex.unlock t.m
end

(* ------------------------------------------------------------------------
   Interpolation bilinéaire en virgule fixe (poids sur 8 bits). *)

(* Pour chaque coordonnée de destination : indice source de gauche, de
   droite, et poids du second (0..256). *)
type axis = { i0 : int array; i1 : int array; f : int array }

let axis ~dst ~src =
  let i0 = Array.make dst 0 and i1 = Array.make dst 0 and f = Array.make dst 0 in
  for x = 0 to dst - 1 do
    let s = ((float x +. 0.5) *. float src /. float dst) -. 0.5 in
    let s = Float.max 0. (Float.min s (float (src - 1))) in
    let a = int_of_float s in
    i0.(x) <- a;
    i1.(x) <- min (a + 1) (src - 1);
    f.(x) <- int_of_float ((s -. float a) *. 256.)
  done;
  { i0; i1; f }

(* Redimensionne en remplissant (w, h), en rognant ce qui dépasse. *)
let cover src w h =
  let dst = create w h in
  let scale = Float.max (float w /. float src.w) (float h /. float src.h) in
  let ox = (float src.w -. (float w /. scale)) /. 2.
  and oy = (float src.h -. (float h /. scale)) /. 2. in
  let coord o n i =
    let s = ((float i +. 0.5) /. scale) +. o -. 0.5 in
    let s = Float.max 0. (Float.min s (float (n - 1))) in
    let a = int_of_float s in
    (a, min (a + 1) (n - 1), int_of_float ((s -. float a) *. 256.))
  in
  let xs = Array.init w (coord ox src.w) in
  for y = 0 to h - 1 do
    let y0, y1, fy = coord oy src.h y in
    for x = 0 to w - 1 do
      let x0, x1, fx = xs.(x) in
      for c = 0 to 2 do
        let p yy xx = get src.d ((((yy * src.w) + xx) * 3) + c) in
        let top = (p y0 x0 * (256 - fx)) + (p y0 x1 * fx)
        and bot = (p y1 x0 * (256 - fx)) + (p y1 x1 * fx) in
        set dst.d ((((y * w) + x) * 3) + c) (((top * (256 - fy)) + (bot * fy)) lsr 16)
      done
    done
  done;
  dst

(* ------------------------------------------------------------------------
   Flou : réduction d'un facteur [shrink], flou en boîte répété (proche d'un
   gaussien), l'agrandissement se fait pendant la composition. *)

let shrink = 8

type blur = { small : t; tmp : t; radius : int }

let blur_create w h ~strength =
  let sw = w / shrink and sh = h / shrink in
  { small = create sw sh; tmp = create sw sh; radius = max 1 (strength / shrink) }

(* Flou en boîte horizontal de [src] vers [dst] ; [stride] et [len]
   permettent de réutiliser la même passe pour les colonnes. *)
let box_pass ~radius ~lines ~len ~line_step ~px_step (src : Ffi.bytes8)
    (dst : Ffi.bytes8) =
  let win = (2 * radius) + 1 in
  for l = 0 to lines - 1 do
    let base = l * line_step in
    for c = 0 to 2 do
      let at i = get src (base + (max 0 (min (len - 1) i) * px_step) + c) in
      let sum = ref 0 in
      for i = -radius to radius do
        sum := !sum + at i
      done;
      for i = 0 to len - 1 do
        set dst (base + (i * px_step) + c) (!sum / win);
        sum := !sum + at (i + radius + 1) - at (i - radius)
      done
    done
  done

let blur_update b (frame : t) =
  let s = b.small in
  (* Quatre échantillons par bloc suffisent : le flou efface le reste. *)
  let q = shrink / 4 in
  for y = 0 to s.h - 1 do
    for x = 0 to s.w - 1 do
      let x0 = (x * shrink) + q and y0 = (y * shrink) + q in
      let x1 = x0 + (2 * q) and y1 = y0 + (2 * q) in
      for c = 0 to 2 do
        let p yy xx = get frame.d ((((yy * frame.w) + xx) * 3) + c) in
        set s.d ((((y * s.w) + x) * 3) + c)
          ((p y0 x0 + p y0 x1 + p y1 x0 + p y1 x1) lsr 2)
      done
    done
  done;
  let row = s.w * 3 in
  for _ = 1 to 3 do
    box_pass ~radius:b.radius ~lines:s.h ~len:s.w ~line_step:row ~px_step:3 s.d
      b.tmp.d;
    box_pass ~radius:b.radius ~lines:s.w ~len:s.h ~line_step:3 ~px_step:row
      b.tmp.d s.d
  done

(* ------------------------------------------------------------------------
   Composition : dst = fg * masque + bg * (1 - masque).
   Le masque (0..255, basse résolution) est agrandi à la volée. *)

type compositor = {
  pool : Pool.t;
  w : int;
  h : int;
  mw : int;
  mx : axis;
  my : axis;
  bw : int;
  bx : axis;
  by : axis;
}

let compositor pool ~w ~h ~mw ~mh =
  let bw = w / shrink and bh = h / shrink in
  {
    pool;
    w;
    h;
    mw;
    mx = axis ~dst:w ~src:mw;
    my = axis ~dst:h ~src:mh;
    bw;
    bx = axis ~dst:w ~src:bw;
    by = axis ~dst:h ~src:bh;
  }

type background = Full of t | Small of t

let composite k ~(fg : t) ~bg ~(mask : Ffi.bytes8) ~(dst : t) =
  let w = k.w in
  let fgd = fg.d and dd = dst.d in
  Pool.rows k.pool k.h (fun lo hi ->
      for y = lo to hi - 1 do
        let r0 = k.my.i0.(y) * k.mw and r1 = k.my.i1.(y) * k.mw in
        let fy = k.my.f.(y) in
        let b0 = k.by.i0.(y) * k.bw * 3 and b1 = k.by.i1.(y) * k.bw * 3 in
        let gy = k.by.f.(y) in
        for x = 0 to w - 1 do
          let x0 = k.mx.i0.(x) and x1 = k.mx.i1.(x) and fx = k.mx.f.(x) in
          let top = (get mask (r0 + x0) * (256 - fx)) + (get mask (r0 + x1) * fx)
          and bot = (get mask (r1 + x0) * (256 - fx)) + (get mask (r1 + x1) * fx) in
          let m = ((top * (256 - fy)) + (bot * fy)) lsr 16 in
          let m = m + (m lsr 7) in
          let i = ((y * w) + x) * 3 in
          if m = 256 then (
            set dd i (get fgd i);
            set dd (i + 1) (get fgd (i + 1));
            set dd (i + 2) (get fgd (i + 2)))
          else
            match bg with
            | Full b ->
                let bd = b.d in
                for c = 0 to 2 do
                  set dd (i + c)
                    (((get fgd (i + c) * m) + (get bd (i + c) * (256 - m))) lsr 8)
                done
            | Small b ->
                let bd = b.d in
                let c0 = k.bx.i0.(x) * 3 and c1 = k.bx.i1.(x) * 3 in
                let gx = k.bx.f.(x) in
                for c = 0 to 2 do
                  let top =
                    (get bd (b0 + c0 + c) * (256 - gx)) + (get bd (b0 + c1 + c) * gx)
                  and bot =
                    (get bd (b1 + c0 + c) * (256 - gx)) + (get bd (b1 + c1 + c) * gx)
                  in
                  let v = ((top * (256 - gy)) + (bot * gy)) lsr 16 in
                  set dd (i + c) (((get fgd (i + c) * m) + (v * (256 - m))) lsr 8)
                done
        done
      done)

(* ------------------------------------------------------------------------
   Échantillonnage vers les entrées des modèles (flottants 0..1). *)

(* Valeur bilinéaire du canal [c] au point (x, y), noir hors de l'image. *)
let sample (img : t) x y c =
  if x < 0. || y < 0. || x > float (img.w - 1) || y > float (img.h - 1) then 0.
  else
    let x0 = int_of_float x and y0 = int_of_float y in
    let x1 = min (x0 + 1) (img.w - 1) and y1 = min (y0 + 1) (img.h - 1) in
    let fx = x -. float x0 and fy = y -. float y0 in
    let p yy xx = float (get img.d ((((yy * img.w) + xx) * 3) + c)) in
    let top = (p y0 x0 *. (1. -. fx)) +. (p y0 x1 *. fx)
    and bot = (p y1 x0 *. (1. -. fx)) +. (p y1 x1 *. fx) in
    ((top *. (1. -. fy)) +. (bot *. fy)) /. 255.

(* Remplit [dst] (n x n x 3) en lisant [img] par la transformation affine
   (u, v) dans [0, n) -> (ox + u*ax + v*bx, oy + u*ay + v*by). *)
let warp (img : t) (dst : Ffi.floats) n ~ox ~oy ~ax ~ay ~bx ~by =
  for v = 0 to n - 1 do
    for u = 0 to n - 1 do
      let fu = float u +. 0.5 and fv = float v +. 0.5 in
      let x = ox +. (fu *. ax) +. (fv *. bx) -. 0.5
      and y = oy +. (fu *. ay) +. (fv *. by) -. 0.5 in
      let i = ((v * n) + u) * 3 in
      for c = 0 to 2 do
        Array1.unsafe_set dst (i + c) (sample img x y c)
      done
    done
  done

(* ------------------------------------------------------------------------
   Fichiers *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let load_jpeg path =
  let data = read_file path in
  let w, h = Ffi.Jpeg.size data in
  let img = create w h in
  Ffi.Jpeg.decode data img.d w h;
  img

let save_jpeg path (img : t) = Ffi.Jpeg.write path img.d img.w img.h

(* Fondu : [dst] = [a] * (1 - k) + [b] * k, k entre 0 et 1. *)
let blend (a : t) (b : t) k (dst : t) =
  let k = max 0 (min 256 (int_of_float (k *. 256.))) in
  for i = 0 to Array1.dim dst.d - 1 do
    set dst.d i (((get a.d i * (256 - k)) + (get b.d i * k)) lsr 8)
  done

let copy (img : t) =
  let c = create img.w img.h in
  Array1.blit img.d c.d;
  c

(* Assombrit [img] sur place, linéairement, et davantage les couleurs
   chaudes : un pixel de chaleur C = max(0, rouge - bleu) / 255 est multiplié
   par gain * k^(1 + warm * C). Là où [keep] (un octet par pixel) est blanc, la
   luminosité est gardée, avec une transition selon le gris. *)
let dim ?keep ?(gain = 1.) (img : t) k ~warm =
  let lut =
    Array.init 256 (fun c ->
        int_of_float (gain *. (k ** (1. +. (warm *. float c /. 255.))) *. 256.))
  in
  for p = 0 to (img.w * img.h) - 1 do
    let i = p * 3 in
    let r = get img.d i and g = get img.d (i + 1) and b = get img.d (i + 2) in
    let f = lut.(max 0 (r - b)) in
    let f =
      match keep with
      | None -> f
      | Some keep ->
          let m = get keep p in
          ((f * (255 - m)) + (256 * m)) / 255
    in
    set img.d i ((r * f) lsr 8);
    set img.d (i + 1) ((g * f) lsr 8);
    set img.d (i + 2) ((b * f) lsr 8)
  done

(* Couleur moyenne de [img] échantillonnée sur une grille de 64x64 points,
   pondérée par le masque [mask] (msize x msize) : la personne (poids m), ou
   le fond si [inv] (poids 255 - m). Renvoie (r, v, b, poids total). *)
let masked_mean (img : t) (mask : Ffi.bytes8) msize ~inv =
  let n = 64 in
  let r = ref 0 and g = ref 0 and b = ref 0 and wt = ref 0 in
  for j = 0 to n - 1 do
    let my = ((2 * j) + 1) * msize / (2 * n) and iy = ((2 * j) + 1) * img.h / (2 * n) in
    for k = 0 to n - 1 do
      let mx = ((2 * k) + 1) * msize / (2 * n) and ix = ((2 * k) + 1) * img.w / (2 * n) in
      let m = get mask ((my * msize) + mx) in
      let m = if inv then 255 - m else m in
      let i = ((iy * img.w) + ix) * 3 in
      r := !r + (m * get img.d i);
      g := !g + (m * get img.d (i + 1));
      b := !b + (m * get img.d (i + 2));
      wt := !wt + m
    done
  done;
  let f = float (max 1 !wt) in
  (float !r /. f, float !g /. f, float !b /. f, !wt)

(* [dst] = [src] multiplié canal par canal par (gr, gg, gb). *)
let gains (src : t) (dst : t) (gr, gg, gb) =
  let lut gn = Array.init 256 (fun c -> min 255 (int_of_float ((float c *. gn) +. 0.5))) in
  let lr = lut gr and lg = lut gg and lb = lut gb in
  let s = src.d and d = dst.d in
  for p = 0 to (src.w * src.h) - 1 do
    let i = p * 3 in
    set d i (Array.unsafe_get lr (get s i));
    set d (i + 1) (Array.unsafe_get lg (get s (i + 1)));
    set d (i + 2) (Array.unsafe_get lb (get s (i + 2)))
  done

(* [dst] = [src] retourné : miroir gauche/droite si [h], haut/bas si [v]. *)
let flip (src : t) (dst : t) ~h ~v =
  let w = src.w and ht = src.h in
  let s = src.d and d = dst.d in
  for y = 0 to ht - 1 do
    let sy = if v then ht - 1 - y else y in
    if h then
      for x = 0 to w - 1 do
        let i = ((sy * w) + (w - 1 - x)) * 3 and o = ((y * w) + x) * 3 in
        set d o (get s i);
        set d (o + 1) (get s (i + 1));
        set d (o + 2) (get s (i + 2))
      done
    else
      Bigarray.Array1.blit
        (Bigarray.Array1.sub s (sy * w * 3) (w * 3))
        (Bigarray.Array1.sub d (y * w * 3) (w * 3))
  done
