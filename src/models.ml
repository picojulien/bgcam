(* Modèles MediaPipe, téléchargés une fois dans le cache. *)

let base = "https://storage.googleapis.com/mediapipe-models/"

let cache =
  let root =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when d <> "" -> d
    | _ -> Filename.concat (Sys.getenv "HOME") ".cache"
  in
  Filename.concat root "bgcam"

let sh fmt =
  Printf.ksprintf
    (fun cmd ->
      if Sys.command cmd <> 0 then failwith ("échec de la commande : " ^ cmd))
    fmt

let path name = Filename.concat cache name
let q = Filename.quote

let segmenter () =
  let p = path "selfie_segmenter.tflite" in
  if not (Sys.file_exists p) then (
    prerr_endline "bgcam: téléchargement du modèle de détourage";
    sh "mkdir -p %s && curl -sfL -o %s.part %s && mv %s.part %s" (q cache) (q p)
      (q (base ^ "image_segmenter/selfie_segmenter/float16/latest/selfie_segmenter.tflite"))
      (q p) (q p));
  p

type hands = {
  detector : string;
  landmarks : string;
  embedder : string;
  classifier : string;
}

(* Le fichier .task est une archive zip qui contient deux autres archives. *)
let hands () =
  let dir = path "gesture" in
  let h =
    {
      detector = Filename.concat dir "hand_detector.tflite";
      landmarks = Filename.concat dir "hand_landmarks_detector.tflite";
      embedder = Filename.concat dir "gesture_embedder.tflite";
      classifier = Filename.concat dir "canned_gesture_classifier.tflite";
    }
  in
  if not (Sys.file_exists h.classifier && Sys.file_exists h.detector) then (
    prerr_endline "bgcam: téléchargement des modèles de gestes";
    let task = path "gesture_recognizer.task" in
    sh "mkdir -p %s && curl -sfL -o %s %s" (q dir) (q task)
      (q (base ^ "gesture_recognizer/gesture_recognizer/float16/latest/gesture_recognizer.task"));
    sh "cd %s && unzip -qo %s && unzip -qo hand_landmarker.task && unzip -qo \
        hand_gesture_recognizer.task && rm -f *.task %s"
      (q dir) (q task) (q task));
  h
