(* Fenêtre de réglage (--ui) : aperçu de la sortie, curseurs qui modifient
   [Live.t] en direct, choix du fond, enregistrement dans la configuration.
   Tourne dans son propre domaine ; tout le dessin est fait ici dans un
   tampon RGB, SDL ne sert qu'à l'afficher (ui_stubs.c). *)

external sdl_open : string -> int -> int -> int -> unit = "bgcam_ui_open"
external sdl_present : Ffi.bytes8 -> unit = "bgcam_ui_present"

external sdl_text : Ffi.bytes8 -> int * int -> int * int -> string -> int -> int
  = "bgcam_ui_text"

external sdl_poll : unit -> int * int * int = "bgcam_ui_poll"
external sdl_close : unit -> unit = "bgcam_ui_close"

(* max et min sur les entiers : ceux de Stdlib sont polymorphes et passent
   par une comparaison générique, coûteuse dans les boucles sur les pixels. *)
let max (a : int) b = if a > b then a else b
let min (a : int) b = if a < b then a else b

let ww = 1110
let wh = 830

(* Aperçu, en 16:9. *)
let vx = 12
let vy = 12
let vw = 768
let vh = 432

(* Panneau des curseurs. *)
let px = vx + vw + 24
let pw = ww - px - 16

let c_back = 0x1d1f24
let c_panel = 0x2a2d35
let c_track = 0x4a4f5c
let c_accent = 0x4fa3ff
let c_text = 0xe6e6e6
let c_dim = 0x9aa0aa
let c_button = 0x3a3e4a

let canvas = Ffi.bytes8 (ww * wh * 3)

(* Accès directs aux octets (Img.get et Img.set ne sont pas inlinés entre
   modules sans flambda). *)
let get (d : Ffi.bytes8) i = Bigarray.Array1.unsafe_get d i
let set (d : Ffi.bytes8) i v = Bigarray.Array1.unsafe_set d i v

let rect x y w h c =
  let r = (c lsr 16) land 255 and g = (c lsr 8) land 255 and b = c land 255 in
  for yy = max 0 y to min (wh - 1) (y + h - 1) do
    for xx = max 0 x to min (ww - 1) (x + w - 1) do
      let i = ((yy * ww) + xx) * 3 in
      set canvas i r;
      set canvas (i + 1) g;
      set canvas (i + 2) b
    done
  done

let text ?(c = c_text) x y s = sdl_text canvas (ww, wh) (x, y) s c

(* Aperçu réduit au plus proche voisin. *)
let draw_preview (p : Img.t) =
  for y = 0 to vh - 1 do
    let sy = y * p.h / vh in
    for x = 0 to vw - 1 do
      let s = ((sy * p.w) + (x * p.w / vw)) * 3 and d = (((vy + y) * ww) + vx + x) * 3 in
      set canvas d (get p.d s);
      set canvas (d + 1) (get p.d (s + 1));
      set canvas (d + 2) (get p.d (s + 2))
    done
  done

(* ------------------------------------------------------------------------
   Commandes *)

type slider = {
  label : string;
  lo : float;
  hi : float;
  step : float;
  get : unit -> float;
  set : float -> unit;
  fmt : float -> string;
  sy : int; (* haut de la commande *)
}

let track_x = px + 10
let track_w = pw - 20
let slider_h = 46
let track_dy = 30

type button = {
  blabel : unit -> string;
  bx : int;
  by : int;
  bw : int;
  bh : int;
  act : unit -> unit;
  on : unit -> bool; (* mis en évidence *)
}

let clamp lo hi v = Float.max lo (Float.min hi v)

let set_slider s v =
  let v = Float.round ((v -. s.lo) /. s.step) *. s.step +. s.lo in
  s.set (clamp s.lo s.hi v)

let slider_at sliders x y =
  List.find_opt
    (fun s -> x >= px && x < px + pw && y >= s.sy && y < s.sy + slider_h)
    sliders

let value_at s x =
  s.lo +. ((s.hi -. s.lo) *. clamp 0. 1. (float (x - track_x) /. float track_w))

let draw_slider s =
  let v = s.get () in
  ignore (text (px + 10) s.sy s.label);
  let vt = s.fmt v in
  ignore (text ~c:c_accent (px + pw - 10 - (9 * String.length vt)) s.sy vt);
  let ty = s.sy + track_dy in
  rect track_x ty track_w 6 c_track;
  let k = int_of_float ((v -. s.lo) /. (s.hi -. s.lo) *. float track_w) in
  rect track_x ty k 6 c_accent;
  rect (track_x + k - 6) (ty - 6) 12 18 c_text

let draw_button b =
  rect b.bx b.by b.bw b.bh (if b.on () then c_accent else c_button);
  let label = b.blabel () in
  ignore (text (b.bx + 10) (b.by + ((b.bh - 20) / 2)) label)

let button_at buttons x y =
  List.find_opt
    (fun b -> x >= b.bx && x < b.bx + b.bw && y >= b.by && y < b.by + b.bh)
    buttons

(* ------------------------------------------------------------------------
   Fenêtre *)

let run (live : Live.t) ~config_path ~backgrounds (preview : Img.t) =
  sdl_open "bgcam — réglages" ww wh 16;
  let pct v = Printf.sprintf "%.2f" v and sec v = Printf.sprintf "%.2f s" v in
  let y0 = 44 in
  (* Sections : détourage (0-3), fond (4-8), gestes (9-12). *)
  let section_gap = 40 in
  let top i = y0 + (i * slider_h) + (if i >= 4 then section_gap else 0) + if i >= 9 then section_gap else 0 in
  let mk i label lo hi step get set fmt = { label; lo; hi; step; get; set; fmt; sy = top i } in
  let sliders =
    [
      mk 0 "Entrée dans la silhouette" 0. 1. 0.01
        (fun () -> live.enter)
        (fun v ->
          live.enter <- v;
          if live.leave > v then live.leave <- v)
        pct;
      mk 1 "Sortie de la silhouette" 0. 1. 0.01
        (fun () -> live.leave)
        (fun v ->
          live.leave <- v;
          if live.enter < v then live.enter <- v)
        pct;
      mk 2 "Réactivité (nouvelle trame)" 0.05 1. 0.01
        (fun () -> live.rate)
        (fun v -> live.rate <- v)
        pct;
      mk 3 "Force du flou" 2. 80. 1.
        (fun () -> float live.blur)
        (fun v -> live.blur <- int_of_float v)
        (fun v -> Printf.sprintf "%.0f" v);
      mk 4 "Luminosité du fond" 0. 1. 0.01
        (fun () -> live.brightness)
        (fun v -> live.brightness <- v)
        (fun v -> Printf.sprintf "%.0f %%" (v *. 100.));
      mk 5 "Luminosité quand je sors" 0. 1. 0.01
        (fun () -> live.absent_brightness)
        (fun v -> live.absent_brightness <- v)
        (fun v -> Printf.sprintf "%.0f %%" (v *. 100.));
      mk 6 "Délai avant tamisage" 0. 15. 0.5
        (fun () -> live.absent_delay)
        (fun v -> live.absent_delay <- v)
        (fun v -> Printf.sprintf "%.1f s" v);
      mk 7 "Atténuation des tons chauds" 0. 5. 0.1
        (fun () -> live.absent_warm)
        (fun v -> live.absent_warm <- v)
        (fun v -> Printf.sprintf "%.2f" v);
      mk 8 "Accord personne / fond" 0. 1. 0.05
        (fun () -> live.match_light)
        (fun v -> live.match_light <- v)
        pct;
      mk 9 "Maintien du geste" 0. 1.5 0.05
        (fun () -> live.hold)
        (fun v -> live.hold <- v)
        sec;
      mk 10 "Délai entre gestes" 0. 5. 0.1
        (fun () -> live.cooldown)
        (fun v -> live.cooldown <- v)
        sec;
      mk 11 "Confiance min. (prédéfinis)" 0. 1. 0.01
        (fun () -> live.min_score)
        (fun v -> live.min_score <- v)
        pct;
      mk 12 "Distance max. (personnels)" 0. 0.6 0.01
        (fun () -> live.max_distance)
        (fun v -> live.max_distance <- v)
        pct;
    ]
  in
  let status = ref "" and status_until = ref 0. in
  let save () =
    (try
       Config.save config_path (Live.to_config live);
       status := "Enregistré dans " ^ Filename.basename config_path
     with Sys_error e -> status := "Échec : " ^ e);
    status_until := Unix.gettimeofday () +. 3.
  in
  let by = vy + vh + 84 in
  let bg_buttons =
    List.mapi
      (fun i name ->
        {
          blabel = (fun () -> name);
          bx = vx + (i * 128);
          by;
          bw = 120;
          bh = 34;
          act = (fun () -> Atomic.set live.request (Some name));
          on = (fun () -> live.current = name);
        })
      backgrounds
  in
  let buttons =
    bg_buttons
    @ [
        {
          blabel = (fun () -> "Fond : refaire (état suivant)");
          bx = vx;
          by = by + 44;
          bw = 290;
          bh = 34;
          act = (fun () -> Atomic.set live.request (Some live.current));
          on = (fun () -> false);
        };
        {
          blabel = (fun () -> "Miroir ↔");
          bx = vx + 302;
          by = by + 44;
          bw = 150;
          bh = 34;
          act = (fun () -> live.flip_h <- not live.flip_h);
          on = (fun () -> live.flip_h);
        };
        {
          blabel = (fun () -> "Haut / bas ↕");
          bx = vx + 462;
          by = by + 44;
          bw = 150;
          bh = 34;
          act = (fun () -> live.flip_v <- not live.flip_v);
          on = (fun () -> live.flip_v);
        };
        {
          blabel = (fun () -> "Enregistrer les réglages");
          bx = px;
          by = wh - 50;
          bw = pw;
          bh = 36;
          act = save;
          on = (fun () -> false);
        };
      ]
  in
  let dragging = ref None and pressed = ref None in
  let running = ref true in
  while !running do
    let began = Unix.gettimeofday () in
    let rec events () =
      match sdl_poll () with
      | 0, _, _ -> ()
      | 1, _, _ -> running := false
      | 2, x, y ->
          (match slider_at sliders x y with
          | Some s ->
              dragging := Some s;
              set_slider s (value_at s x)
          | None -> pressed := button_at buttons x y);
          events ()
      | 3, x, y ->
          (match (!pressed, button_at buttons x y) with
          | Some b, Some b' when b == b' -> b.act ()
          | _ -> ());
          dragging := None;
          pressed := None;
          events ()
      | 4, x, _ ->
          Option.iter (fun s -> set_slider s (value_at s x)) !dragging;
          events ()
      | ((5 | 6) as k), x, y ->
          Option.iter
            (fun s -> set_slider s (s.get () +. if k = 5 then s.step else -.s.step))
            (slider_at sliders x y);
          events ()
      | _ -> events ()
    in
    events ();
    if !running then begin
      rect 0 0 ww wh c_back;
      draw_preview preview;
      rect (px - 8) 0 (pw + 16) wh c_panel;
      ignore (text ~c:c_dim (px + 10) 14 "DÉTOURAGE");
      ignore (text ~c:c_dim (px + 10) (top 4 - section_gap + 14) "FOND");
      ignore (text ~c:c_dim (px + 10) (top 9 - section_gap + 14) "GESTES");
      List.iter draw_slider sliders;
      List.iter draw_button buttons;
      let ty = vy + vh + 10 in
      ignore
        (text vx ty
           (Printf.sprintf "Fond : %s    %.0f i/s    silhouette %.0f %%" live.current
              live.fps (live.presence *. 100.)));
      ignore (text ~c:c_dim vx (ty + 22) ("Vu : " ^ live.seen));
      ignore (text ~c:c_dim vx (ty + 44) ("Dernier geste : " ^ live.fired));
      if Unix.gettimeofday () < !status_until then
        ignore (text ~c:c_accent px (wh - 76) !status);
      sdl_present canvas;

      Unix.sleepf (Float.max 0. ((1. /. 30.) -. (Unix.gettimeofday () -. began)))
    end
  done;
  sdl_close ()

let start live ~config_path ~backgrounds preview =
  ignore
    (Domain.spawn (fun () ->
         try run live ~config_path ~backgrounds preview
         with Failure e -> prerr_endline ("bgcam: fenêtre de réglage : " ^ e)))
