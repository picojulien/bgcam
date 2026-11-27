(* bgcam : lit une webcam, détoure la personne, floute ou remplace le fond,
   et publie le résultat sur une webcam virtuelle (v4l2loopback). Les gestes
   de la main changent le fond. *)

let now = Unix.gettimeofday

(* Messages horodatés (heure locale, à la milliseconde) : on peut relier un
   geste détecté au moment où il a été fait. *)
let log fmt =
  Printf.ksprintf
    (fun s ->
      let t = now () in
      let tm = Unix.localtime t in
      Printf.eprintf "bgcam %02d:%02d:%02d.%03d: %s\n%!" tm.tm_hour tm.tm_min tm.tm_sec
        (int_of_float ((t -. Float.of_int (truncate t)) *. 1000.))
        s)
    fmt

(* ------------------------------------------------------------------------
   Fonds *)

(* Fond animé préchargé, joué en boucle selon la durée de chaque trame. *)
type loop = { frames : Img.t array; ends : float array (* secondes *) }

let max_loop_frames = 600

let load_loop path w h =
  match String.lowercase_ascii (Filename.extension path) with
  | ".jpg" | ".jpeg" ->
      { frames = [| Img.cover (Img.load_jpeg path) w h |]; ends = [| 1. |] }
  | ".webp" ->
      let dec, sw, sh = Ffi.Webp.open_ (Img.read_file path) in
      let raw = Img.create sw sh in
      let rec read acc n =
        (* Toujours lire jusqu'au bout : c'est ce qui libère le décodeur. *)
        let ts = Ffi.Webp.next dec raw.d in
        if ts < 0 then List.rev acc
        else if n >= max_loop_frames then read acc n
        else read ((Img.cover raw w h, float ts /. 1000.) :: acc) (n + 1)
      in
      let frames = read [] 0 in
      if frames = [] then failwith (path ^ " : aucune trame");
      {
        frames = Array.of_list (List.map fst frames);
        ends = Array.of_list (List.map (fun (_, t) -> Float.max t 0.001) frames);
      }
  | _ -> failwith (path ^ " : format non pris en charge (WebP ou JPEG)")

(* Vidéo (WebM, MP4…) décodée en continu par ffmpeg, en boucle, au rythme
   de la vidéo. Un domaine lit les trames ; la boucle principale prend la
   plus récente. ffmpeg ne tourne que tant que le fond est affiché. *)
type video = {
  pid : int;
  vm : Mutex.t;
  mutable front : Img.t; (* dernière trame complète *)
  primed : bool Atomic.t; (* [front] contient une trame *)
  ended : bool Atomic.t; (* fin du fichier (lecture sans boucle) *)
}

let video_exts = [ ".webm"; ".mp4"; ".mkv"; ".mov"; ".gif"; ".avi" ]

(* [loop] : relit le fichier sans fin. [init] : image affichée tant que la
   première trame n'est pas décodée. *)
let start_video ?(loop = true) ?init path w h =
  let rd, wr = Unix.pipe ~cloexec:true () in
  let filter =
    Printf.sprintf "scale=%d:%d:force_original_aspect_ratio=increase,crop=%d:%d" w h w h
  in
  let args =
    Array.of_list
      ([ "ffmpeg"; "-hide_banner"; "-loglevel"; "error"; "-nostdin"; "-re" ]
      @ (if loop then [ "-stream_loop"; "-1" ] else [])
      @ [ "-i"; path; "-vf"; filter; "-pix_fmt"; "rgb24"; "-f"; "rawvideo"; "pipe:1" ])
  in
  let pid =
    try Unix.create_process "ffmpeg" args Unix.stdin wr Unix.stderr
    with Unix.Unix_error _ -> failwith "ffmpeg introuvable (nécessaire pour les vidéos)"
  in
  Unix.close wr;
  let v =
    {
      pid;
      vm = Mutex.create ();
      front = (match init with Some i -> Img.copy i | None -> Img.create w h);
      primed = Atomic.make (init <> None);
      ended = Atomic.make false;
    }
  in
  let back = ref (Img.create w h) in
  let size = w * h * 3 in
  let rec fill off =
    off = size
    || (let n = Unix.read_bigarray rd !back.d off (size - off) in
        n > 0 && fill (off + n))
  in
  ignore
    (Domain.spawn (fun () ->
         (try
            while fill 0 do
              Mutex.lock v.vm;
              let f = v.front in
              v.front <- !back;
              back := f;
              Mutex.unlock v.vm;
              Atomic.set v.primed true
            done
          with Unix.Unix_error _ -> ());
         Unix.close rd;
         ignore (Unix.waitpid [] pid);
         Atomic.set v.ended true));
  v

(* SIGKILL : ffmpeg ne tente pas de finir d'écrire dans un tuyau fermé. *)
let stop_video v = try Unix.kill v.pid Sys.sigkill with Unix.Unix_error _ -> ()

(* Copie de la trame courante. *)
let snapshot v = Mutex.protect v.vm (fun () -> Img.copy v.front)

(* Scène : un dossier de clips. Les clips sans tiret sont des états, joués
   en boucle (ordre alphabétique, le premier au départ) ; un clip "a-b"
   est la transition de l'état a vers l'état b. Redemander la scène passe
   à l'état suivant, par sa transition si elle existe. *)
type scene = {
  clips : (string * string) list; (* nom -> chemin *)
  states : string array;
  mutable state : int;
  mutable target : int option; (* transition en cours vers cet état *)
  mutable player : video option;
  mutable fade_from : Img.t option; (* fondu depuis cette image… *)
  mutable fade_t0 : float; (* …commencé à cet instant *)
  keep : Ffi.bytes8 option;
      (* masque.jpg : zones non tamisées en l'absence de personne *)
}

let fade_time = 0.6

let load_scene dir w h =
  let keep =
    let path = Filename.concat dir "masque.jpg" in
    if not (Sys.file_exists path) then None
    else
      let m = Img.cover (Img.load_jpeg path) w h in
      let k = Ffi.bytes8 (w * h) in
      for p = 0 to (w * h) - 1 do
        Img.set k p (Img.get m.d (p * 3))
      done;
      Some k
  in
  let clips =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f ->
           List.mem (String.lowercase_ascii (Filename.extension f)) video_exts)
    |> List.sort compare
    |> List.map (fun f -> (Filename.remove_extension f, Filename.concat dir f))
  in
  let states =
    List.filter_map (fun (n, _) -> if String.contains n '-' then None else Some n) clips
  in
  if states = [] then failwith (dir ^ " : aucun clip d'état (nom sans tiret)");
  {
    clips;
    states = Array.of_list states;
    state = 0;
    target = None;
    player = None;
    fade_from = None;
    fade_t0 = 0.;
    keep;
  }

let scene_play sc ?(loop = true) ?init ?(fade = false) clip w h =
  Option.iter stop_video sc.player;
  sc.player <- Some (start_video ~loop ?init (List.assoc clip sc.clips) w h);
  sc.fade_from <- (if fade then init else None);
  sc.fade_t0 <- now ()

let scene_stop sc =
  Option.iter stop_video sc.player;
  sc.player <- None;
  sc.target <- None

let scene_enter sc w h =
  sc.state <- 0;
  sc.target <- None;
  scene_play sc sc.states.(0) w h

(* Passe à l'état suivant ; sans effet pendant une transition. *)
let scene_advance sc w h =
  if sc.target = None then begin
    let next = (sc.state + 1) mod Array.length sc.states in
    let init = Option.map snapshot sc.player in
    let jump = sc.states.(sc.state) ^ "-" ^ sc.states.(next) in
    log "scène : %s -> %s" sc.states.(sc.state) sc.states.(next);
    if List.mem_assoc jump sc.clips then (
      sc.target <- Some next;
      scene_play sc ~loop:false ?init ~fade:true jump w h)
    else (
      sc.state <- next;
      scene_play sc ?init ~fade:true sc.states.(next) w h)
  end

(* À chaque trame : une transition terminée laisse place à la boucle de
   l'état atteint, en reprenant sa dernière image. *)
let scene_tick sc w h =
  match (sc.target, sc.player) with
  | Some next, Some v when Atomic.get v.ended ->
      sc.state <- next;
      sc.target <- None;
      scene_play sc ~init:(snapshot v) sc.states.(next) w h
  | _ -> ()

type source = Loop of loop | Video of string * video option ref | Scene of scene

let loop_frame l t =
  let n = Array.length l.frames in
  let t = Float.rem t l.ends.(n - 1) in
  let rec find i = if i >= n - 1 || t < l.ends.(i) then i else find (i + 1) in
  l.frames.(find 0)

type backgrounds = {
  cfg : Config.t;
  order : string list; (* blur, fonds configurés, none *)
  loops : (string, source) Hashtbl.t;
  mutable current : string;
  mutable since : float;
  mutable last_scene : string option; (* dernière scène affichée *)
}

let rec apply b action =
  match action with
  | "next" | "prev" ->
      let n = List.length b.order in
      let i = Option.get (List.find_index (( = ) b.current) b.order) in
      let j = if action = "next" then i + 1 else i + n - 1 in
      apply b (List.nth b.order (j mod n))
  | name when not (List.mem name b.order) -> log "fond inconnu : %S" name
  | name ->
      (match List.assoc_opt name b.cfg.backgrounds with
      | Some path when not (Hashtbl.mem b.loops name) ->
          let ext = String.lowercase_ascii (Filename.extension path) in
          Hashtbl.replace b.loops name
            (if Sys.file_exists path && Sys.is_directory path then Scene (load_scene path b.cfg.width b.cfg.height)
             else if List.mem ext video_exts then (
               if not (Sys.file_exists path) then failwith (path ^ " : fichier absent");
               Video (path, ref None))
             else Loop (load_loop path b.cfg.width b.cfg.height))
      | _ -> ());
      if name <> b.current then (
        log "fond -> %s" name;
        b.since <- now ();
        (* La vidéo quittée s'arrête, celle choisie repart du début. *)
        (match Hashtbl.find_opt b.loops b.current with
        | Some (Video (_, r)) ->
            Option.iter stop_video !r;
            r := None
        | Some (Scene sc) -> scene_stop sc
        | _ -> ());
        (match Hashtbl.find_opt b.loops name with
        | Some (Scene _) -> b.last_scene <- Some name
        | _ -> ());
        match Hashtbl.find_opt b.loops name with
        | Some (Video (path, r)) when !r = None ->
            r := Some (start_video path b.cfg.width b.cfg.height)
        | Some (Scene sc) -> scene_enter sc b.cfg.width b.cfg.height
        | _ -> ())
      else (
        match Hashtbl.find_opt b.loops name with
        | Some (Scene sc) -> scene_advance sc b.cfg.width b.cfg.height
        | _ -> ());
      b.current <- name

(* Action d'un geste. Un geste lié à une scène, fait pendant qu'une autre
   scène est affichée (deux variantes d'un même décor, par exemple), fait
   avancer la scène affichée au lieu d'en changer ; depuis un fond qui n'est
   pas une scène, il ramène la dernière scène affichée. *)
let gesture_apply b action =
  let is_scene name =
    match List.assoc_opt name b.cfg.backgrounds with
    | Some p -> Sys.file_exists p && Sys.is_directory p
    | None -> false
  in
  if action <> b.current && is_scene action then
    if is_scene b.current then apply b b.current
    else
      (* Depuis le flou ou l'image brute : retour à la dernière scène vue. *)
      apply b (match b.last_scene with Some s when is_scene s -> s | _ -> action)
  else apply b action

let backgrounds (cfg : Config.t) =
  let order = ("blur" :: List.map fst cfg.backgrounds) @ [ "none" ] in
  let b =
    { cfg; order; loops = Hashtbl.create 8; current = "blur"; since = now (); last_scene = None }
  in
  apply b cfg.default_bg;
  b

(* ------------------------------------------------------------------------
   Gestes : reconnus dans un domaine séparé, sur la trame la plus récente,
   pour ne pas ralentir la boucle vidéo. *)

type watcher = {
  m : Mutex.t;
  ready : Condition.t;
  shared : Img.t;
  mutable fresh : bool;
  mutable idle : bool;
  action : string option Atomic.t;
  finished : bool Atomic.t; (* fin de l'enregistrement *)
}

let debug_gestures = ref false

(* Enregistrement d'un geste : [samples] plongements, pris après un délai
   pour laisser le temps de se mettre en place. *)
type record = { name : string; samples : int }

let record_delay = 3.

(* Attend une trame fraîche et la copie dans [local]. *)
let take w local =
  Mutex.lock w.m;
  w.idle <- true;
  while not w.fresh do
    Condition.wait w.ready w.m
  done;
  Bigarray.Array1.blit w.shared.d local.Img.d;
  w.fresh <- false;
  w.idle <- false;
  Mutex.unlock w.m

let record_gesture hands w (r : record) =
  let local = Img.create w.shared.w w.shared.h in
  let start = now () in
  log "enregistrement de %S dans %.0f s : prendre la pose, puis la varier un \
       peu (inclinaison, distance, main gauche ou droite)" r.name record_delay;
  let acc = ref [] and n = ref 0 in
  while !n < r.samples do
    take w local;
    let began = now () in
    if began -. start >= record_delay && Hands.embed hands local then (
      acc := Hands.embedding hands :: !acc;
      incr n;
      if !n mod 10 = 0 then log "%d / %d" !n r.samples);
    Unix.sleepf (Float.max 0. (0.05 -. (now () -. began)))
  done;
  let path = Custom.save r.name (List.rev !acc) in
  log "%d exemples ajoutés à %s" r.samples path;
  Atomic.set w.finished true

(* Un geste déclenche une fois, après avoir été tenu [hold] secondes, puis
   plus rien pendant [cooldown] secondes. Les gestes personnels passent
   avant les gestes prédéfinis quand ils sont assez proches. *)
(* Durée d'absence d'un geste avant qu'il puisse redéclencher. *)
let release = 0.8

let watch_gestures (cfg : Config.t) (live : Live.t) hands w =
  let custom = Custom.load ~classify:(fun v -> fst (Hands.classify_vec hands v)) in
  if not (Custom.is_empty custom) then
    log "gestes personnels : %s"
      (String.concat ", "
         (List.map
            (fun g -> Printf.sprintf "%s (vu comme %s)" g (Custom.usual custom g))
            (Custom.gestures custom)));
  let local = Img.create w.shared.w w.shared.h in
  let candidate = ref None and since = ref 0. in
  let quiet_until = ref 0. and last_seen = ref "" in
  (* Un geste déclenche une fois, puis doit être absent [release] secondes
     (main perdue, ou autre geste vu à sa place) avant de redéclencher :
     une pose tenue que la reconnaissance fait hésiter entre deux gestes ne
     redéclenche pas. *)
  let last_hit = Hashtbl.create 8 and fired = Hashtbl.create 8 in
  while true do
    take w local;
    let began = now () in
    let seen, desc =
      if not (Hands.embed hands local) then (None, "pas de main")
      else
        let mine = Custom.nearest custom (Hands.embedding hands) in
        let name, score = Hands.classify hands in
        let desc =
          Printf.sprintf "%s %.1f%s" name score
            (match mine with
            | Some (n, v, d) -> Printf.sprintf " | perso %s %.0f%% d=%.2f" n (v *. 100.) d
            | None -> "")
        in
        (* Un geste prédéfini reconnu avec assurance l'emporte sur un geste
           personnel dont les exemples ressemblent à autre chose. *)
        let canned = name <> "None" && score >= live.min_score in
        match mine with
        | Some (n, v, d)
          when d <= live.max_distance && v > 0.5
               && not (canned && name <> Custom.usual custom n) ->
            (Some n, desc)
        | _ when canned -> (Some name, desc)
        | _ -> (None, desc)
    in
    live.seen <- desc;
    if !debug_gestures && desc <> !last_seen then (
      log "vu : %s" desc;
      last_seen := desc);
    let t = now () in
    (* Réarme les gestes absents depuis [release] secondes. *)
    Hashtbl.filter_map_inplace
      (fun name () ->
        match Hashtbl.find_opt last_hit name with
        | Some last when t -. last < release -> Some ()
        | _ -> None)
      fired;
    Option.iter (fun name -> Hashtbl.replace last_hit name t) seen;
    (if seen <> !candidate then (
       candidate := seen;
       since := t);
     match seen with
     | Some name when t >= !quiet_until && t -. !since >= live.hold
                     && not (Hashtbl.mem fired name) ->
         Hashtbl.replace fired name ();
         quiet_until := t +. live.cooldown;
         let action = List.assoc_opt name cfg.gesture_map in
         log "geste %s -> %s" name (Option.value action ~default:"(non associé)");
         live.fired <- Printf.sprintf "%s -> %s" name (Option.value action ~default:"(non associé)");
         if action <> None then Atomic.set w.action action
     | _ -> ());
    (* Dix analyses par seconde quand une main est visible, cinq sinon :
       le détecteur de paume est le modèle le plus coûteux, et la main est
       absente la plupart du temps. *)
    let period = if desc = "pas de main" then 0.2 else 0.1 in
    Unix.sleepf (Float.max 0. (period -. (now () -. began)))
  done

let watch cfg live record w =
  let hands = Hands.create () in
  match record with
  | Some r -> record_gesture hands w r
  | None -> watch_gestures cfg live hands w

let start_watcher cfg live record w h =
  let w =
    {
      m = Mutex.create ();
      ready = Condition.create ();
      shared = Img.create w h;
      fresh = false;
      idle = false;
      action = Atomic.make None;
      finished = Atomic.make false;
    }
  in
  ignore (Domain.spawn (fun () -> watch cfg live record w));
  w

(* Donne la trame au domaine des gestes s'il attend, sans jamais bloquer. *)
let offer w (frame : Img.t) =
  if Mutex.try_lock w.m then (
    if w.idle && not w.fresh then (
      Bigarray.Array1.blit frame.d w.shared.d;
      w.fresh <- true;
      Condition.signal w.ready);
    Mutex.unlock w.m)

(* ------------------------------------------------------------------------
   Boucle principale *)

let run ~config ~bg ~camera ~flip ~no_output ~input ~snapshot ~max_frames ~record ~ui =
  let cfg = Config.load config in
  let cfg = match camera with Some d -> { cfg with cam_device = d } | None -> cfg in
  let cfg = match flip with Some f -> { cfg with flip = f } | None -> cfg in
  if not (List.mem cfg.flip [ "none"; "horizontal"; "h"; "vertical"; "v"; "both"; "hv" ]) then
    failwith (Printf.sprintf "flip = %S : none, horizontal, vertical ou both" cfg.flip);
  (* Source : webcam, ou image fixe pour les essais. *)
  let next_frame, (frame : Img.t) =
    match input with
    | Some path when List.mem (String.lowercase_ascii (Filename.extension path)) video_exts ->
        (* Vidéo lue en boucle par ffmpeg, à la place de la webcam. *)
        if not (Sys.file_exists path) then failwith (path ^ " : fichier absent");
        let v = start_video path cfg.width cfg.height in
        while not (Atomic.get v.primed || Atomic.get v.ended) do
          Unix.sleepf 0.01
        done;
        if not (Atomic.get v.primed) then failwith (path ^ " : vidéo illisible");
        let img = Img.create cfg.width cfg.height in
        (* Cadence de la config, temps de traitement compris. *)
        let due = ref (Unix.gettimeofday ()) in
        let next () =
          due := Float.max (Unix.gettimeofday ()) (!due +. (1. /. float cfg.fps));
          Unix.sleepf (Float.max 0. (!due -. Unix.gettimeofday ()));
          Mutex.protect v.vm (fun () -> Bigarray.Array1.blit v.front.d img.d);
          not (Atomic.get v.ended)
        in
        ignore (next ());
        (next, img)
    | Some path ->
        let img = Img.cover (Img.load_jpeg path) cfg.width cfg.height in
        ((fun () -> Unix.sleepf (1. /. float cfg.fps); true), img)
    | None ->
        let cam, w, h = Ffi.Cam.open_ cfg.cam_device cfg.width cfg.height cfg.fps in
        if (w, h) <> (cfg.width, cfg.height) then
          log "la webcam impose %dx%d au lieu de %dx%d" w h cfg.width cfg.height;
        let img = Img.create w h in
        ((fun () -> Ffi.Cam.read cam img.d), img)
  in
  let cfg = { cfg with width = frame.w; height = frame.h } in
  let w = cfg.width and h = cfg.height in

  (* --bg accepte aussi un chemin (image, vidéo, dossier de scène) : il est
     ajouté aux fonds sous son nom de fichier, gestes next/prev compris. *)
  let cfg, bg =
    match bg with
    | Some p when (not (List.mem_assoc p cfg.backgrounds)) && Sys.file_exists p ->
        let base = Filename.remove_extension (Filename.basename p) in
        let name = if List.mem_assoc base cfg.backgrounds then p else base in
        ({ cfg with backgrounds = cfg.backgrounds @ [ (name, p) ] }, Some name)
    | _ -> (cfg, bg)
  in
  let bgs = backgrounds cfg in
  Option.iter (apply bgs) bg;
  let live = Live.of_config cfg in
  let seg = Seg.create live in
  (* Le flou est recréé quand sa force change dans la fenêtre de réglage. *)
  let blur = ref (Img.blur_create w h ~strength:live.blur) and blur_strength = ref live.blur in
  let comp = Img.compositor (Img.Pool.create 4) ~w ~h ~mw:Seg.size ~mh:Seg.size in
  let result = Img.create w h in
  let fading = Img.create w h and dimmed = Img.create w h in
  (* Personne accordée au fond : gains par canal, lissés dans le temps. *)
  let lit = Img.create w h and light = ref (1., 1., 1.) in
  let flipped = Img.create w h in
  let match_light (bg : Img.t) mask =
    let pr, pg, pb, pw = Img.masked_mean frame mask Seg.size ~inv:false in
    let br, bgr, bb, bw = Img.masked_mean bg mask Seg.size ~inv:true in
    if pw < 64 * 64 * 255 / 50 || bw < 64 * 64 * 255 / 50 then None
    else
      let k = live.match_light in
      let luma r g b = Float.max 8. ((0.299 *. r) +. (0.587 *. g) +. (0.114 *. b)) in
      let yp = luma pr pg pb and yb = luma br bgr bb in
      (* Surtout la luminosité, bornée ; la teinte à peine (10 % au plus). *)
      let l = Float.min 1.25 (Float.max 0.6 ((yb /. yp) ** k)) in
      let tint p b =
        let c = (Float.max 4. b /. yb) /. (Float.max 4. p /. yp) in
        l *. Float.min 1.1 (Float.max 0.9 (c ** (0.4 *. k)))
      in
      let tr = tint pr br and tg = tint pg bgr and tb = tint pb bb in
      let r, g, b = !light and a = 0.05 in
      light := (r +. (a *. (tr -. r)), g +. (a *. (tg -. g)), b +. (a *. (tb -. b)));
      Img.gains frame lit !light;
      Some lit
  in
  let watcher =
    if record <> None || (cfg.gestures && cfg.gesture_map <> []) then
      Some (start_watcher cfg live record w h)
    else None
  in
  let out =
    if no_output || (record <> None && not (Sys.file_exists cfg.out_device)) then None
    else if not (Sys.file_exists cfg.out_device) then
      failwith
        (Printf.sprintf
           "%s n'existe pas. Créer la webcam virtuelle :\n\
           \  sudo modprobe v4l2loopback devices=1 video_nr=10 \
            card_label=bgcam exclusive_caps=1\n\
            ou lancer avec --no-output pour essayer sans."
           cfg.out_device)
    else (
      log "sortie sur %s (%dx%d)" cfg.out_device w h;
      Some (Ffi.Out.open_ cfg.out_device w h cfg.fps))
  in

  let preview =
    if ui then (
      let p = Img.create w h in
      Ui.start live ~config_path:(Option.value config ~default:Config.default_path)
        ~backgrounds:bgs.order p;
      Some p)
    else None
  in
  let start = now () and n = ref 0 and shown = ref frame in
  let tick = ref (now ()) in
  (* Absence : sans personne détectée pendant [absent_delay] s, le fond
     (image, vidéo, scène) baisse jusqu'à [absent_brightness]. *)
  let last_present = ref (now ()) and brightness = ref 1. and away = ref false in
  let report () =
    let dt = now () -. start in
    log "%d trames en %.1f s (%.1f i/s), fond %s" !n dt (float !n /. dt) bgs.current
  in
  (* Ctrl-C ou SIGTERM : la boucle s'arrête proprement à la trame suivante,
     quel que soit le domaine qui reçoit le signal. *)
  let stop = Atomic.make false in
  List.iter
    (fun s -> Sys.set_signal s (Sys.Signal_handle (fun _ -> Atomic.set stop true)))
    [ Sys.sigint; Sys.sigterm ];
  while
       (not (Atomic.get stop))
       && (match max_frames with Some m -> !n < m | None -> true)
       && not (match watcher with Some wt -> Atomic.get wt.finished | None -> false)
     do
       if next_frame () then begin
         Option.iter
           (fun wt ->
             offer wt frame;
             Option.iter (gesture_apply bgs) (Atomic.exchange wt.action None))
           watcher;
         Option.iter (apply bgs) (Atomic.exchange live.request None);
         if live.blur <> !blur_strength then (
           blur_strength := live.blur;
           blur := Img.blur_create w h ~strength:live.blur);
         let blur = !blur in
         (* Absence : sans personne détectée (silhouette sous 3 % de l'image à
            la trame précédente) pendant [absent_delay] s, le fond baisse vers
            [absent_brightness] en 1,5 s, et remonte en 1 s au retour. *)
         (let t = now () and dt = now () -. !tick in
          live.presence <- seg.coverage;
          if seg.coverage > 0.03 then last_present := t;
          let absent = t -. !last_present > live.absent_delay in
          if absent <> !away then (
            away := absent;
            log (if absent then "personne absente : fond tamisé" else "personne de retour"));
          let target = if absent then live.absent_brightness else 1. in
          brightness :=
            if target < !brightness then Float.max target (!brightness -. (dt /. 1.5))
            else Float.min target (!brightness +. dt));
         (* Fond image ou vidéo, assombri si besoin (présence ou absence) dans
            une copie : ni la personne ni les zones du masque de la scène ne
            sont touchées. *)
         let full ?keep (img : Img.t) =
           let img =
             if !brightness > 0.999 && live.brightness > 0.999 then img
             else (
               Bigarray.Array1.blit img.d dimmed.d;
               Img.dim ?keep ~gain:live.brightness dimmed !brightness ~warm:live.absent_warm;
               dimmed)
           in
           (* Fond retourné (pas la personne). *)
           if live.flip_h || live.flip_v then (
             Img.flip img flipped ~h:live.flip_h ~v:live.flip_v;
             Img.Full flipped)
           else Img.Full img
         in
         (shown :=
            match bgs.current with
            | "none" -> frame
            | name ->
                let mask = Seg.mask seg frame in
                let draw bg =
                  let fg =
                    match bg with
                    | Img.Full b when live.match_light > 0.01 -> (
                        match match_light b mask with Some l -> l | None -> frame)
                    | _ ->
                        light := (1., 1., 1.);
                        frame
                  in
                  Img.composite comp ~fg ~bg ~mask ~dst:result
                in
                (if name = "blur" then (
                   Img.blur_update blur frame;
                   draw (Img.Small blur.small))
                 else
                   match Hashtbl.find bgs.loops name with
                   | Loop l -> draw (full (loop_frame l (now () -. bgs.since)))
                   | Video (_, r) ->
                       (* La trame ne doit pas changer pendant la composition. *)
                       let v = Option.get !r in
                       draw (Mutex.protect v.vm (fun () -> full v.front))
                   | Scene sc -> (
                       scene_tick sc w h;
                       match sc.player with
                       | Some v when Atomic.get v.primed ->
                           let keep = sc.keep in
                           let bg =
                             Mutex.protect v.vm (fun () ->
                                 let k = (now () -. sc.fade_t0) /. fade_time in
                                 match sc.fade_from with
                                 | Some from when k < 1. ->
                                     Img.blend from v.front k fading;
                                     full ?keep fading
                                 | _ -> full ?keep v.front)
                           in
                           draw bg
                       | _ ->
                           (* Avant la première trame : le flou. *)
                           Img.blur_update blur frame;
                           draw (Img.Small blur.small)));
                result);
         Option.iter (fun o -> Ffi.Out.write o !shown.d) out;
         Option.iter (fun (p : Img.t) -> Bigarray.Array1.blit !shown.d p.d) preview;
         let t = now () in
         live.fps <- (0.9 *. live.fps) +. (0.1 /. Float.max 1e-3 (t -. !tick));
         tick := t;
         live.current <- bgs.current;
         incr n;
         if !n mod 900 = 0 then report ()
       end
     done;
  Hashtbl.iter
    (fun _ -> function
      | Video (_, { contents = Some v }) -> stop_video v
      | Scene sc -> scene_stop sc
      | _ -> ())
    bgs.loops;
  if !n > 0 then report ();
  Option.iter (fun path -> Img.save_jpeg path !shown) snapshot

let () =
  let config = ref None and bg = ref None and no_output = ref false in
  let input = ref None and snapshot = ref None and max_frames = ref None in
  let camera = ref None and flip = ref None in
  let record = ref None and samples = ref 100 and ui = ref false in
  let opt r = Arg.String (fun s -> r := Some s) in
  let spec =
    [
      ("--config", opt config, "FICHIER  configuration (défaut : " ^ Config.default_path ^ ")");
      ("--bg", opt bg, "NOM  fond de départ : blur, none, un nom configuré ou un chemin (image, vidéo, dossier de scène)");
      ("--no-output", Arg.Set no_output, " n'écrit pas sur la webcam virtuelle");
      ("--camera", opt camera, "DEV  webcam à lire (défaut : [camera] device, /dev/video0)");
      ("--flip", opt flip, "SENS  retourne le fond (pas la personne) : none, horizontal (miroir), vertical (haut/bas) ou both");
      ("--input", opt input, "FICHIER  vidéo (lue en boucle) ou image JPEG traitée à la place de la webcam");
      ("--snapshot", opt snapshot, "JPEG  enregistre la dernière trame produite (essais)");
      ("--ui", Arg.Set ui, " ouvre la fenêtre de réglage (aperçu et réglages en direct)");
      ("--debug-gestures", Arg.Set debug_gestures, " journalise ce que voit la reconnaissance de gestes");
      ("--record", opt record, "NOM  enregistre un geste personnel puis s'arrête");
      ("--samples", Arg.Set_int samples, "N  nombre d'exemples à enregistrer (défaut : 100)");
      ("--frames", Arg.Int (fun i -> max_frames := Some i), "N  s'arrête après N trames (essais)");
    ]
  in
  Arg.parse (Arg.align spec)
    (fun a -> raise (Arg.Bad ("argument inattendu : " ^ a)))
    "bgcam [options]";
  try
    run ~config:!config ~bg:!bg ~camera:!camera ~flip:!flip ~no_output:!no_output ~input:!input
      ~snapshot:!snapshot ~max_frames:!max_frames ~ui:!ui
      ~record:
        (Option.map
           (fun name ->
             if not (Custom.valid_name name) then
               failwith "nom de geste : lettres, chiffres, _ et - seulement";
             { name; samples = max 1 !samples })
           !record)
  with Failure msg ->
    prerr_endline ("bgcam: " ^ msg);
    exit 1
