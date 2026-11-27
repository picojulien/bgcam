# bgcam

Webcam virtuelle pour Linux : bgcam lit la webcam, détoure la personne,
floute le fond ou le remplace (image, vidéo en boucle, scène animée), et
publie le résultat sur une caméra virtuelle utilisable dans Meet, Zoom,
Jitsi… Des gestes de la main changent le fond.

Écrit en OCaml 5. Le C (`src/stubs.c`, `src/ui_stubs.c`) se limite aux appels
de bibliothèques système : TFLite (inférence), V4L2 (capture et sortie),
libjpeg, libwebp, SDL2 (fenêtre de réglage).

## Installation

Testé sur Debian 13 et testing (« forky »). Les noms de paquets ci-dessous
sont ceux de Debian ; ailleurs, chercher les mêmes bibliothèques.

### 1. Dépendances système

```bash
sudo apt install build-essential pkg-config curl unzip ffmpeg \
  libtensorflow-lite-dev libjpeg-dev libwebp-dev \
  libsdl2-dev libsdl2-ttf-dev fonts-dejavu-core \
  v4l2loopback-dkms linux-headers-$(uname -r)
```

- `libtensorflow-lite-dev` : inférence des modèles MediaPipe (TFLite 2.14).
- `ffmpeg` : lecture des fonds vidéo (WebM, MP4…).
- `v4l2loopback-dkms` : la caméra virtuelle. DKMS compile le module pour le
  noyau en cours, d'où les en-têtes `linux-headers-$(uname -r)`. Avec Secure
  Boot, le module doit être signé (DKMS propose d'enrôler une clé MOK à
  l'installation).

### 2. OCaml (5.2 ou plus) et dune

bgcam n'utilise aucune bibliothèque OCaml hors de la distribution standard ;
il suffit du compilateur et de dune. Avec opam :

```bash
sudo apt install opam
opam init                      # une fois
opam switch create bgcam 5.3.0
eval $(opam env --switch=bgcam)
opam install dune
```

Si la distribution fournit déjà OCaml ≥ 5.2 et dune, ils conviennent.

### 3. Compilation

```bash
git clone git@github.com:picojulien/bgcam.git
cd bgcam
dune build
dune install                   # installe `bgcam` dans le switch opam
```

Sans `dune install`, le programme est `_build/default/src/main.exe`.

### 4. Caméra virtuelle

Créer la caméra (à refaire après chaque redémarrage) :

```bash
sudo modprobe v4l2loopback devices=1 video_nr=10 card_label=bgcam exclusive_caps=1
```

Pour qu'elle existe à chaque démarrage :

```bash
echo v4l2loopback | sudo tee /etc/modules-load.d/v4l2loopback.conf
echo 'options v4l2loopback devices=1 video_nr=10 card_label=bgcam exclusive_caps=1' \
  | sudo tee /etc/modprobe.d/v4l2loopback.conf
```

`exclusive_caps=1` est nécessaire pour que Chrome et Firefox la voient comme
une caméra.

### 5. Configuration

```bash
mkdir -p ~/.config/bgcam
cp config.toml ~/.config/bgcam/
```

Le fichier est commenté ; les fonds (`[backgrounds]`) et les gestes
(`[gestures.map]`) s'y déclarent. Sans fichier, bgcam floute le fond.

### 6. Premier essai

Sans webcam ni caméra virtuelle, sur une photo :

```bash
bgcam --input photo.jpg --no-output --frames 60 --snapshot sortie.jpg
```

Le premier lancement télécharge les modèles MediaPipe (environ 9 Mo) dans
`~/.cache/bgcam`.

Puis pour de vrai, avec la fenêtre de réglage :

```bash
bgcam --ui
```

Dans Meet, choisir la caméra **bgcam**. Pour voir la sortie sans appel :
`ffplay /dev/video10`.

### Dépannage

| Symptôme | Cause et solution |
|---|---|
| `webcam déjà utilisée par une autre application` | Une seule application peut lire la webcam : fermer l'onglet ou l'application qui l'utilise (`fuser -v /dev/video0` la nomme). Les autres doivent passer par la caméra « bgcam ». |
| `/dev/video10 n'existe pas` | Charger le module (étape 4). |
| « bgcam » absente de la liste des caméras du navigateur | PipeWire a recensé la caméra virtuelle avant que bgcam n'écrive dedans et l'a prise pour une sortie. Lancer bgcam avant le navigateur, ou `systemctl --user restart wireplumber`, puis recharger la page. |
| `Permission denied` sur `/dev/video*` | Ajouter l'utilisateur au groupe `video` (`sudo usermod -aG video $USER`, puis se reconnecter). |
| Image saccadée (15 i/s au lieu de 30) | Souvent la webcam qui ralentit en faible lumière : éclairer davantage. |
| `modprobe: FATAL: Module v4l2loopback not found` | Le module n'est pas compilé pour ce noyau : réinstaller `linux-headers-$(uname -r)` puis `sudo dpkg-reconfigure v4l2loopback-dkms`. |

## Utilisation

```
bgcam [--config FICHIER] [--bg NOM] [--camera DEV | --input FICHIER] [--ui] [--debug-gestures]
```

- `--bg NOM` : fond de départ (`blur`, `none`, un nom de `[backgrounds]`, ou
  directement un chemin : image, vidéo ou dossier de scène, par exemple
  `--bg ~/Videos/plage.webm`).
- `--ui` : fenêtre de réglage (voir plus bas).
- `--debug-gestures` : journalise ce que voit la reconnaissance de gestes.
- `--record NOM` : enregistre un geste personnel (voir plus bas).
- `--camera DEV` : webcam à lire (défaut : `/dev/video0`, ou `[camera] device`).
- `--input FICHIER` : traite une vidéo (lue en boucle) ou une image JPEG à la
  place de la webcam.
- `--flip SENS` : retourne l'image envoyée, fond compris : `horizontal`
  (miroir), `vertical` (haut/bas) ou `both`. En config : `flip` dans
  `[output]`.
- `--no-output`, `--frames N`, `--snapshot JPEG` : essais.

Ctrl-C arrête bgcam proprement.

## Fonds

Déclarés dans `[backgrounds]` de la configuration :

- image JPEG ou WebP, WebP animé ;
- vidéo (WebM, MP4, MKV, MOV, GIF), lue en boucle par ffmpeg ;
- **scène** : un dossier de clips formant une petite machine à états :

```
vaisseau/
  etoiles.webm             état (boucle), le premier par ordre alphabétique
  nebuleuse.webm           état (boucle)
  etoiles-nebuleuse.webm   transition etoiles -> nebuleuse (jouée une fois)
  nebuleuse-etoiles.webm   transition retour
```

Le geste associé à une scène l'affiche dans son premier état ; refait quand
la scène est affichée, il passe à l'état suivant par la transition (ignoré
pendant une transition). Un fondu de 0,6 s masque le raccord quand une
boucle est interrompue. Un fichier `masque.jpg` facultatif dans le dossier
indique en blanc les zones à ne pas tamiser en l'absence de personne (une
fenêtre sur l'extérieur, par exemple). Les clips ne sont pas fournis : n'importe quelles
vidéos dont les fins et débuts se raccordent conviennent.

Sans geste, `blur` (flou) et `none` (image brute) sont toujours disponibles.

`brightness` (section `[background]`) règle la luminosité du fond derrière
la personne, sans toucher à la personne. Quand personne n'est détecté devant la caméra pendant `absent_delay`
secondes, le fond (image, vidéo ou scène) baisse jusqu'à
`absent_brightness`, puis revient dès le retour.

`match_light` (0 à 1, défaut 0,5) rapproche la luminosité de la personne de
celle du fond visible autour d'elle, et très légèrement sa teinte, pour
qu'elle semble éclairée par la scène. Le gain est borné (de 0,6 à 1,25 pour
la luminosité, ±10 % pour la teinte) et lissé sur une seconde environ. Pas
d'effet avec le flou ni l'image brute.

## Gestes

Gestes prédéfinis : `Open_Palm`, `Closed_Fist`, `Victory`, `Thumb_Up`,
`Thumb_Down`, `Pointing_Up`, `ILoveYou`. Dans `[gestures.map]`, chaque geste
reçoit une action : `blur`, `none`, `next`, `prev` ou un nom de fond.

Un geste déclenche après avoir été tenu `hold` secondes, une seule fois : il
doit disparaître 0,8 s avant de pouvoir redéclencher.

### Gestes personnels

Une pose de la main s'apprend en quelques secondes (bgcam ne doit pas déjà
tourner, il tient la webcam) :

```bash
bgcam --record pouce_index        # 100 exemples ; --samples N pour changer
```

Après 3 s, bgcam enregistre la main visible. Varier un peu la pose pendant
ce temps (inclinaison, distance, main gauche et droite), dans la lumière où
le geste servira. Relancer la même commande ajoute des exemples. Associer
ensuite le geste à une action : `pouce_index = "next"` dans
`[gestures.map]`.

Les exemples sont dans `~/.config/bgcam/gestures/NOM.txt` ; supprimer le
fichier oublie le geste. Un geste personnel est reconnu quand la main est
assez proche de ses exemples (`max_distance`, distance affichée par `--ui` et
`--debug-gestures`) ; un geste prédéfini reconnu avec assurance garde la
priorité s'il diffère de ce que le classifieur voit dans les exemples. Seules
les poses statiques d'une main sont reconnues, pas les mouvements. Choisir
une pose qui ne ressemble pas à la main au repos.

## Fenêtre de réglage

`bgcam --ui` ouvre une fenêtre avec l'aperçu de la sortie, ce que voit la
reconnaissance de gestes, des boutons pour choisir le fond (« refaire » passe
à l'état suivant d'une scène) et des curseurs pour les seuils du détourage,
la force du flou et les réglages des gestes. Les changements s'appliquent
tout de suite ; « Enregistrer les réglages » les écrit dans le fichier de
configuration en gardant ses commentaires. Fermer la fenêtre laisse bgcam
tourner. Les boutons « Miroir ↔ » et « Haut / bas ↕ » retournent l'image envoyée,
fond compris.

## Organisation du code

- `src/main.ml` : fonds, scènes, déclenchement des gestes, boucle principale.
- `src/img.ml` : images, flou, composition (parallélisée sur 4 domaines).
- `src/seg.ml` : détourage (modèle selfie_segmenter, hystérésis contre le
  clignotement).
- `src/hands.ml` : gestes ; réimplémente la chaîne MediaPipe (paume, zone de
  la main, 21 points, plongement, classifieur).
- `src/custom.ml` : gestes personnels (plus proches voisins).
- `src/live.ml`, `src/ui.ml`, `src/ui_stubs.c` : réglages en direct et
  fenêtre de réglage.
- `src/config.ml` : lecture et écriture de la configuration.
- `src/models.ml` : téléchargement des modèles.
- `src/stubs.c` : liaisons C, dont l'opération TFLite
  `Convolution2DTransposeBias` que la bibliothèque Debian ne fournit pas.
- `proto/` : premier prototype Python (MediaPipe officiel), gardé comme
  référence de comparaison.
