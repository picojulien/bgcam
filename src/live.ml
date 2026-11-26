(* Réglages modifiables pendant que bgcam tourne (fenêtre --ui), et état
   montré par cette fenêtre. Lus sans verrou par la boucle vidéo et le
   domaine des gestes : une valeur flottante lue pendant sa modification
   donne l'ancienne ou la nouvelle, ce qui suffit ici. *)

type t = {
  (* détourage *)
  mutable enter : float;
  mutable leave : float;
  mutable rate : float;
  mutable blur : int;
  (* absence *)
  mutable absent_brightness : float;
  mutable absent_delay : float;
  (* gestes *)
  mutable hold : float;
  mutable cooldown : float;
  mutable min_score : float;
  mutable max_distance : float;
  (* état, pour l'affichage *)
  mutable seen : string; (* dernière analyse des gestes *)
  mutable fired : string; (* dernier geste déclenché *)
  mutable current : string; (* fond affiché *)
  mutable fps : float;
  mutable presence : float; (* part de l'image occupée par la personne *)
  (* demande de fond venue de la fenêtre, appliquée par la boucle vidéo *)
  request : string option Atomic.t;
}

let of_config (c : Config.t) =
  {
    enter = c.seg_enter;
    leave = c.seg_leave;
    rate = c.seg_rate;
    blur = c.blur;
    absent_brightness = c.absent_brightness;
    absent_delay = c.absent_delay;
    hold = c.hold;
    cooldown = c.cooldown;
    min_score = c.min_score;
    max_distance = c.max_distance;
    seen = "";
    fired = "";
    current = "";
    fps = 0.;
    presence = 0.;
    request = Atomic.make None;
  }

(* Valeurs à écrire dans le fichier de configuration : section, clé, valeur. *)
let to_config l =
  [
    ("segmentation", "enter", Printf.sprintf "%.2f" l.enter);
    ("segmentation", "leave", Printf.sprintf "%.2f" l.leave);
    ("segmentation", "rate", Printf.sprintf "%.2f" l.rate);
    ("background", "blur", string_of_int l.blur);
    ("background", "absent_brightness", Printf.sprintf "%.2f" l.absent_brightness);
    ("background", "absent_delay", Printf.sprintf "%.1f" l.absent_delay);
    ("gestures", "hold", Printf.sprintf "%.2f" l.hold);
    ("gestures", "cooldown", Printf.sprintf "%.2f" l.cooldown);
    ("gestures", "min_score", Printf.sprintf "%.2f" l.min_score);
    ("gestures", "max_distance", Printf.sprintf "%.2f" l.max_distance);
  ]
