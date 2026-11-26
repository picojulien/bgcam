(* Gestes personnels : plongements de main enregistrés par [bgcam --record],
   reconnus par plus proches voisins.

   Un fichier par geste, [~/.config/bgcam/gestures/NOM.txt], une ligne de
   128 nombres par exemple. Supprimer le fichier oublie le geste. *)

let dir = Filename.concat (Filename.dirname Config.default_path) "gestures"

let valid_name n =
  n <> ""
  && String.for_all
       (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
       n

(* Vecteur ramené à une norme 1 : la distance entre deux plongements est
   alors 1 - cosinus. *)
let unit v =
  let n = sqrt (Array.fold_left (fun a x -> a +. (x *. x)) 0. v) in
  if n = 0. then v else Array.map (fun x -> x /. n) v

let distance a b =
  let d = ref 0. in
  Array.iteri (fun i x -> d := !d +. (x *. b.(i))) a;
  1. -. !d

type t = {
  names : string array;
  vecs : float array array;
  (* Geste prédéfini que le classifieur voit le plus souvent dans les
     exemples de chaque geste personnel (souvent "None"). *)
  usual : (string * string) list;
}

let empty = { names = [||]; vecs = [||]; usual = [] }
let is_empty t = t.vecs = [||]

(* [classify v] : geste prédéfini reconnu dans le plongement [v]. *)
let load ~classify =
  if not (Sys.file_exists dir) then empty
  else
    let files =
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".txt")
      |> List.sort compare
    in
    let samples =
      List.concat_map
        (fun f ->
          let name = Filename.chop_suffix f ".txt" in
          In_channel.with_open_text (Filename.concat dir f) In_channel.input_all
          |> String.split_on_char '\n'
          |> List.filter_map (fun line ->
                 match String.split_on_char ' ' (String.trim line) with
                 | [ "" ] -> None
                 | xs -> Some (name, Array.of_list (List.map float_of_string xs))))
        files
    in
    let names = List.sort_uniq compare (List.map fst samples) in
    let usual =
      List.map
        (fun name ->
          let counts = Hashtbl.create 8 in
          List.iter
            (fun (n, v) ->
              if n = name then
                let l = classify v in
                Hashtbl.replace counts l
                  (1 + Option.value (Hashtbl.find_opt counts l) ~default:0))
            samples;
          let best, _ =
            Hashtbl.fold
              (fun l c (bl, bc) -> if c > bc then (l, c) else (bl, bc))
              counts ("None", 0)
          in
          (name, best))
        names
    in
    {
      names = Array.of_list (List.map fst samples);
      vecs = Array.of_list (List.map (fun (_, v) -> unit v) samples);
      usual;
    }

let gestures t = List.map fst t.usual
let usual t name = List.assoc name t.usual

(* Ajoute des exemples au fichier du geste. *)
let save name (vs : float array list) =
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  let path = Filename.concat dir (name ^ ".txt") in
  Out_channel.with_open_gen [ Open_append; Open_creat; Open_text ] 0o644 path (fun oc ->
      List.iter
        (fun v ->
          Array.iteri
            (fun i x -> Printf.fprintf oc (if i = 0 then "%.6g" else " %.6g") x)
            v;
          output_char oc '\n')
        vs);
  path

let k = 5

(* Geste personnel le plus proche : nom, part des [k] plus proches voisins
   qui le désignent, et distance moyenne de ces voisins. *)
let nearest t v =
  if is_empty t then None
  else
    let v = unit v in
    let d = Array.map (distance v) t.vecs in
    let idx = Array.init (Array.length d) Fun.id in
    Array.sort (fun a b -> compare d.(a) d.(b)) idx;
    let top = Array.sub idx 0 (min k (Array.length idx)) in
    let votes = Hashtbl.create 4 in
    Array.iter
      (fun i ->
        let n, s = Option.value (Hashtbl.find_opt votes t.names.(i)) ~default:(0, 0.) in
        Hashtbl.replace votes t.names.(i) (n + 1, s +. d.(i)))
      top;
    let name, (n, s) =
      Hashtbl.fold
        (fun name (n, s) best ->
          match best with
          | Some (_, (bn, _)) when bn >= n -> best
          | _ -> Some (name, (n, s)))
        votes None
      |> Option.get
    in
    Some (name, float n /. float (Array.length top), s /. float n)
