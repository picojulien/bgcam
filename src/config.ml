(* Configuration : sous-ensemble de TOML (sections, chaînes, nombres,
   booléens), suffisant pour config.toml. *)

type t = {
  cam_device : string;
  width : int;
  height : int;
  fps : int;
  out_device : string;
  default_bg : string;
  blur : int;
  brightness : float; (* luminosité du fond, personne présente *)
  absent_brightness : float; (* luminosité du fond quand personne n'est là *)
  absent_delay : float;
  absent_warm : float; (* atténuation supplémentaire des couleurs chaudes *)
  backgrounds : (string * string) list; (* nom, chemin, dans l'ordre du fichier *)
  gestures : bool;
  hold : float;
  cooldown : float;
  min_score : float;
  max_distance : float; (* gestes personnels *)
  seg_enter : float;
  seg_leave : float;
  seg_rate : float;
  gesture_map : (string * string) list;
}

let default_path =
  let root =
    match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some d when d <> "" -> d
    | _ -> Filename.concat (Sys.getenv "HOME") ".config"
  in
  Filename.concat root "bgcam/config.toml"

let fail path n msg =
  failwith (Printf.sprintf "%s, ligne %d : %s" path n msg)

(* Retire un commentaire qui n'est pas dans une chaîne. *)
let strip_comment line =
  let in_str = ref false and cut = ref (String.length line) in
  String.iteri
    (fun i ch ->
      if ch = '"' then in_str := not !in_str
      else if ch = '#' && (not !in_str) && i < !cut then cut := i)
    line;
  String.trim (String.sub line 0 !cut)

let unquote s =
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2) else s

(* Liste (section, clé, valeur) dans l'ordre du fichier. *)
let parse path =
  let section = ref "" in
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.mapi (fun i l -> (i + 1, strip_comment l))
  |> List.filter_map (fun (n, l) ->
         let len = String.length l in
         if len = 0 then None
         else if l.[0] = '[' then (
           if l.[len - 1] <> ']' then fail path n "section mal formée";
           section := String.trim (String.sub l 1 (len - 2));
           None)
         else
           match String.index_opt l '=' with
           | None -> fail path n "« clé = valeur » attendu"
           | Some i ->
               let key = unquote (String.trim (String.sub l 0 i)) in
               let v = String.trim (String.sub l (i + 1) (len - i - 1)) in
               Some (!section, key, unquote v))

let expand_home p =
  if String.length p >= 2 && String.sub p 0 2 = "~/" then
    Filename.concat (Sys.getenv "HOME") (String.sub p 2 (String.length p - 2))
  else p

let load path =
  let entries =
    match path with
    | Some p -> parse p
    | None -> if Sys.file_exists default_path then parse default_path else []
  in
  let get sec key conv default =
    match List.find_opt (fun (s, k, _) -> s = sec && k = key) entries with
    | None -> default
    | Some (_, _, v) -> (
        try conv v
        with _ -> failwith (Printf.sprintf "[%s] %s : valeur invalide %S" sec key v))
  in
  let section sec =
    List.filter_map (fun (s, k, v) -> if s = sec then Some (k, v) else None) entries
  in
  let str = Fun.id in
  {
    cam_device = get "camera" "device" str "/dev/video0";
    width = get "camera" "width" int_of_string 1280;
    height = get "camera" "height" int_of_string 720;
    fps = get "camera" "fps" int_of_string 30;
    out_device = get "output" "device" str "/dev/video10";
    default_bg = get "background" "default" str "blur";
    blur = get "background" "blur" int_of_string 25;
    brightness = get "background" "brightness" float_of_string 1.;
    absent_brightness = get "background" "absent_brightness" float_of_string 0.35;
    absent_delay = get "background" "absent_delay" float_of_string 3.;
    absent_warm = get "background" "absent_warm" float_of_string 2.;
    backgrounds = List.map (fun (k, v) -> (k, expand_home v)) (section "backgrounds");
    gestures = get "gestures" "enabled" bool_of_string true;
    hold = get "gestures" "hold" float_of_string 0.5;
    cooldown = get "gestures" "cooldown" float_of_string 1.5;
    min_score = get "gestures" "min_score" float_of_string 0.6;
    max_distance = get "gestures" "max_distance" float_of_string 0.15;
    seg_enter = get "segmentation" "enter" float_of_string 0.75;
    seg_leave = get "segmentation" "leave" float_of_string 0.4;
    seg_rate = get "segmentation" "rate" float_of_string 0.4;
    gesture_map = section "gestures.map";
  }

(* Écrit des valeurs dans le fichier [path] en gardant le reste (commentaires
   compris) : remplace la ligne « clé = … » de la section, ou l'ajoute à la
   fin de la section, ou ajoute la section à la fin du fichier. *)
let save path (values : (string * string * string) list) =
  let lines =
    if Sys.file_exists path then
      String.split_on_char '\n' (In_channel.with_open_text path In_channel.input_all)
    else []
  in
  let pending = ref values in
  let out = Buffer.create 4096 in
  let line l = Buffer.add_string out l; Buffer.add_char out '\n' in
  let flush_section sec =
    List.iter (fun (s, k, v) -> if s = sec then line (Printf.sprintf "%s = %s" k v)) !pending;
    pending := List.filter (fun (s, _, _) -> s <> sec) !pending
  in
  let section = ref "" in
  List.iter
    (fun l ->
      let t = strip_comment l in
      let len = String.length t in
      if len > 1 && t.[0] = '[' && t.[len - 1] = ']' then (
        flush_section !section;
        section := String.trim (String.sub t 1 (len - 2));
        line l)
      else
        match String.index_opt t '=' with
        | Some i -> (
            let key = unquote (String.trim (String.sub t 0 i)) in
            match List.find_opt (fun (s, k, _) -> s = !section && k = key) !pending with
            | Some (s, k, v) ->
                pending := List.filter (fun (s', k', _) -> (s', k') <> (s, k)) !pending;
                line (Printf.sprintf "%s = %s" k v)
            | None -> line l)
        | None -> line l)
    lines;
  flush_section !section;
  List.iter
    (fun sec ->
      if List.exists (fun (s, _, _) -> s = sec) !pending then (
        line ("[" ^ sec ^ "]");
        flush_section sec))
    (List.sort_uniq compare (List.map (fun (s, _, _) -> s) !pending));
  (* Pas de ligne vide ajoutée à la fin à chaque enregistrement. *)
  let text = Buffer.contents out in
  let text =
    if String.ends_with ~suffix:"\n\n" text then String.sub text 0 (String.length text - 1)
    else text
  in
  Out_channel.with_open_text path (fun oc -> output_string oc text)
