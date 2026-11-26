# bgcam

Webcam virtuelle : lit la webcam, détoure la personne, floute le fond ou le
remplace par un WebP animé ou une vidéo en boucle, et publie le résultat sur un
périphérique v4l2loopback utilisable dans Meet. Des gestes de la main changent
le fond.

Écrit en OCaml 5. Le C (`src/stubs.c`) se limite aux appels de bibliothèques
système : TFLite (inférence), V4L2 (capture et sortie), libjpeg, libwebp.

## Prérequis

```bash
sudo apt install libtensorflow-lite-dev libjpeg-dev libwebp-dev libsdl2-dev libsdl2-ttf-dev \
  fonts-dejavu-core curl unzip ffmpeg
# webcam virtuelle (demande les en-têtes du noyau en cours d'exécution)
sudo apt install v4l2loopback-dkms linux-headers-amd64
```

Les modèles MediaPipe sont téléchargés au premier lancement dans
`~/.cache/bgcam`.

## Construction

```bash
dune build
dune install   # ou lancer _build/default/src/main.exe
```

## Utilisation

```bash
sudo modprobe v4l2loopback devices=1 video_nr=10 card_label=bgcam exclusive_caps=1
mkdir -p ~/.config/bgcam && cp config.toml ~/.config/bgcam/
bgcam
```

Puis choisir la caméra « bgcam » dans Meet. Pour voir le résultat sans appel :
`ffplay /dev/video10`.

La webcam réelle ne peut être ouverte que par une application à la fois :
elle doit être libre quand bgcam démarre.

Options : `--config FICHIER`, `--bg NOM` (fond de départ). Pour essayer sans
webcam ni périphérique virtuel :

```bash
bgcam --input photo.jpg --no-output --frames 60 --snapshot sortie.jpg
```

## Fenêtre de réglage

```bash
bgcam --ui
```

Ouvre à côté de la webcam virtuelle une fenêtre (SDL2) avec l'aperçu de la
sortie, ce que voit la reconnaissance de gestes, des boutons pour choisir le
fond (« refaire » passe à l'état suivant d'une scène) et des curseurs pour
les seuils du détourage, la force du flou et les réglages des gestes. Les
changements s'appliquent tout de suite ; « Enregistrer les réglages » les
écrit dans le fichier de configuration en gardant ses commentaires. Fermer
la fenêtre laisse bgcam tourner. Molette sur un curseur : un cran.

## Configuration

Voir `config.toml` : fonds nommés (WebP animé ou fixe, JPEG, vidéo lue par
ffmpeg), fond de départ,
force du flou, et table geste -> action.

Gestes reconnus : `Open_Palm`, `Closed_Fist`, `Victory`, `Thumb_Up`,
`Thumb_Down`, `Pointing_Up`, `ILoveYou`. Actions : `blur`, `none`, `next`,
`prev` ou un nom de fond. Un geste déclenche après avoir été tenu `hold`
secondes.

## Scènes

Un fond peut être un dossier de clips vidéo formant une scène à états :

```
vaisseau/
  etoiles.webm             état (boucle), le premier par ordre alphabétique
  nebuleuse.webm           état (boucle)
  etoiles-nebuleuse.webm   transition etoiles -> nebuleuse (jouée une fois)
  nebuleuse-etoiles.webm   transition retour
```

Le geste associé à la scène l'affiche dans son premier état ; refait quand
la scène est affichée, il passe à l'état suivant par la transition (ignoré
pendant une transition). Un fondu de 0,6 s masque le raccord entre une
boucle interrompue et le début de la transition.

## Gestes personnels

Une pose de la main se fait apprendre en quelques secondes :

```bash
bgcam --record pouce_index        # 100 exemples ; --samples N pour changer
```

Après 3 s, bgcam enregistre la main visible. Pendant ce temps, varier un peu
la pose (inclinaison, distance, main gauche et droite) ; `ffplay /dev/video10`
montre l'image si la webcam virtuelle existe. Relancer la même commande
ajoute des exemples. Le geste s'associe ensuite à une action comme les
autres, dans `[gestures.map]` : `pouce_index = "next"`.

Les exemples sont dans `~/.config/bgcam/gestures/NOM.txt` ; supprimer le
fichier oublie le geste. Reconnaissance : les 5 exemples les plus proches du
plongement de la main (vecteur de 128 nombres calculé par le modèle
gesture_embedder) ; le geste personnel l'emporte sur les gestes prédéfinis
si la distance moyenne reste sous `max_distance`, sauf quand le classifieur
prédéfini reconnaît avec assurance un autre geste que celui qu'il voit
d'habitude dans les exemples (un poing proche d'une pose personnelle reste
un poing). `--debug-gestures` affiche
ce qui est vu et la distance. Seules les poses statiques d'une main sont
reconnues, pas les mouvements.

## Organisation

- `src/img.ml` : images, flou, composition (parallélisée sur 4 domaines).
- `src/seg.ml` : détourage (modèle selfie_segmenter).
- `src/hands.ml` : gestes ; réimplémente la chaîne MediaPipe (paume, zone de
  la main, 21 points, plongement, classifieur).
- `src/custom.ml` : gestes personnels (exemples enregistrés, plus proches
  voisins).
- `src/live.ml`, `src/ui.ml`, `src/ui_stubs.c` : réglages en direct et
  fenêtre de réglage.
- `src/main.ml` : fonds, déclenchement des gestes, boucle principale.
- `src/stubs.c` : liaisons C, dont l'opération TFLite
  `Convolution2DTransposeBias` que la bibliothèque Debian ne fournit pas.
- `proto/` : premier prototype Python (MediaPipe officiel), gardé comme
  référence de comparaison ; supprimable.
