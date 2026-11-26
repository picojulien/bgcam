"""bgcam : lit une webcam, détoure la personne, floute ou remplace le fond,
et publie le résultat sur une webcam virtuelle (v4l2loopback).

Les gestes de la main peuvent changer le fond en cours de route.
"""

from __future__ import annotations

import argparse
import os
import sys
import threading
import time
import tomllib
import urllib.request
from dataclasses import dataclass, field
from pathlib import Path

import cv2
import mediapipe as mp
import numpy as np
from mediapipe.tasks import python as mp_tasks
from mediapipe.tasks.python import vision
from PIL import Image, ImageSequence

MODELS = {
    "selfie_segmenter.tflite": "https://storage.googleapis.com/mediapipe-models/"
    "image_segmenter/selfie_segmenter/float16/latest/selfie_segmenter.tflite",
    "gesture_recognizer.task": "https://storage.googleapis.com/mediapipe-models/"
    "gesture_recognizer/gesture_recognizer/float16/latest/gesture_recognizer.task",
}
CACHE = Path(os.environ.get("XDG_CACHE_HOME", "~/.cache")).expanduser() / "bgcam"
DEFAULT_CONFIG = (
    Path(os.environ.get("XDG_CONFIG_HOME", "~/.config")).expanduser()
    / "bgcam"
    / "config.toml"
)
BUILTIN = ("blur", "none")
# Largeur à laquelle tourne la segmentation ; le masque est ensuite agrandi.
SEG_WIDTH = 512
MAX_BG_FRAMES = 600


def model_path(name: str) -> str:
    path = CACHE / name
    if not path.exists():
        CACHE.mkdir(parents=True, exist_ok=True)
        print(f"bgcam: téléchargement du modèle {name}", file=sys.stderr)
        tmp = path.with_suffix(".part")
        urllib.request.urlretrieve(MODELS[name], tmp)
        tmp.rename(path)
    return str(path)


# --------------------------------------------------------------------------
# Configuration


@dataclass
class Config:
    cam_device: str = "/dev/video0"
    width: int = 1280
    height: int = 720
    fps: int = 30
    out_device: str = "/dev/video10"
    default_bg: str = "blur"
    blur: int = 25
    backgrounds: dict[str, str] = field(default_factory=dict)
    gestures_enabled: bool = True
    hold: float = 0.5
    cooldown: float = 1.5
    min_score: float = 0.6
    gesture_map: dict[str, str] = field(default_factory=dict)


def load_config(path: Path | None) -> Config:
    cfg = Config()
    if path is None:
        if not DEFAULT_CONFIG.exists():
            return cfg
        path = DEFAULT_CONFIG
    with open(path, "rb") as f:
        raw = tomllib.load(f)
    cam, out, bg, ges = (
        raw.get(k, {}) for k in ("camera", "output", "background", "gestures")
    )
    cfg.cam_device = cam.get("device", cfg.cam_device)
    cfg.width = cam.get("width", cfg.width)
    cfg.height = cam.get("height", cfg.height)
    cfg.fps = cam.get("fps", cfg.fps)
    cfg.out_device = out.get("device", cfg.out_device)
    cfg.default_bg = bg.get("default", cfg.default_bg)
    cfg.blur = bg.get("blur", cfg.blur)
    cfg.backgrounds = dict(raw.get("backgrounds", {}))
    cfg.gestures_enabled = ges.get("enabled", cfg.gestures_enabled)
    cfg.hold = ges.get("hold", cfg.hold)
    cfg.cooldown = ges.get("cooldown", cfg.cooldown)
    cfg.min_score = ges.get("min_score", cfg.min_score)
    cfg.gesture_map = dict(ges.get("map", {}))
    return cfg


# --------------------------------------------------------------------------
# Fonds


def cover(img: np.ndarray, w: int, h: int) -> np.ndarray:
    """Redimensionne en remplissant (w, h), en rognant ce qui dépasse."""
    ih, iw = img.shape[:2]
    scale = max(w / iw, h / ih)
    nw, nh = max(w, round(iw * scale)), max(h, round(ih * scale))
    img = cv2.resize(img, (nw, nh), interpolation=cv2.INTER_AREA)
    x, y = (nw - w) // 2, (nh - h) // 2
    return np.ascontiguousarray(img[y : y + h, x : x + w])


class Loop:
    """Fond animé préchargé en mémoire, joué en boucle selon ses durées."""

    def __init__(self, path: Path, w: int, h: int):
        self.frames: list[np.ndarray] = []
        durations: list[float] = []
        try:
            with Image.open(path) as im:
                for frame in ImageSequence.Iterator(im):
                    if len(self.frames) >= MAX_BG_FRAMES:
                        break
                    bgr = cv2.cvtColor(
                        np.asarray(frame.convert("RGB")), cv2.COLOR_RGB2BGR
                    )
                    self.frames.append(cover(bgr, w, h))
                    durations.append((frame.info.get("duration") or 40) / 1000)
        except (OSError, ValueError):
            cap = cv2.VideoCapture(str(path))
            step = 1 / (cap.get(cv2.CAP_PROP_FPS) or 25)
            while len(self.frames) < MAX_BG_FRAMES:
                ok, bgr = cap.read()
                if not ok:
                    break
                self.frames.append(cover(bgr, w, h))
                durations.append(step)
            cap.release()
        if not self.frames:
            raise SystemExit(f"bgcam: fond illisible : {path}")
        self.ends = np.cumsum(durations)

    def at(self, t: float) -> np.ndarray:
        i = int(np.searchsorted(self.ends, t % self.ends[-1], side="right"))
        return self.frames[min(i, len(self.frames) - 1)]


class Backgrounds:
    """Fond courant et actions pour en changer. Chargement à la demande."""

    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.order = [*BUILTIN[:1], *cfg.backgrounds, BUILTIN[1]]
        self.loops: dict[str, Loop] = {}
        self.current = "blur"
        self.since = time.monotonic()
        self.apply(cfg.default_bg)

    def apply(self, action: str) -> None:
        if action in ("next", "prev"):
            i = self.order.index(self.current)
            action = self.order[(i + (1 if action == "next" else -1)) % len(self.order)]
        if action not in self.order:
            print(f"bgcam: fond inconnu : {action!r}", file=sys.stderr)
            return
        if action not in BUILTIN and action not in self.loops:
            path = Path(self.cfg.backgrounds[action]).expanduser()
            self.loops[action] = Loop(path, self.cfg.width, self.cfg.height)
        if action != self.current:
            print(f"bgcam: fond -> {action}", file=sys.stderr)
            self.since = time.monotonic()
        self.current = action

    def render(self, frame: np.ndarray) -> np.ndarray | None:
        """Image de fond pour cette trame, ou None s'il ne faut rien changer."""
        if self.current == "none":
            return None
        if self.current == "blur":
            h, w = frame.shape[:2]
            small = cv2.resize(frame, (w // 4, h // 4), interpolation=cv2.INTER_AREA)
            k = max(1, self.cfg.blur // 4) * 2 + 1
            small = cv2.GaussianBlur(small, (k, k), 0)
            return cv2.resize(small, (w, h), interpolation=cv2.INTER_LINEAR)
        return self.loops[self.current].at(time.monotonic() - self.since)


# --------------------------------------------------------------------------
# Segmentation


class Segmenter:
    def __init__(self):
        self.seg = vision.ImageSegmenter.create_from_options(
            vision.ImageSegmenterOptions(
                base_options=mp_tasks.BaseOptions(
                    model_asset_path=model_path("selfie_segmenter.tflite")
                ),
                running_mode=vision.RunningMode.VIDEO,
                output_confidence_masks=True,
                output_category_mask=False,
            )
        )
        self.prev: np.ndarray | None = None

    def mask(self, frame: np.ndarray, ts_ms: int) -> np.ndarray:
        """Masque flottant (0 = fond, 1 = personne) à la taille de la trame."""
        h, w = frame.shape[:2]
        sw = min(SEG_WIDTH, w)
        sh = round(h * sw / w)
        small = cv2.cvtColor(cv2.resize(frame, (sw, sh)), cv2.COLOR_BGR2RGB)
        res = self.seg.segment_for_video(
            mp.Image(image_format=mp.ImageFormat.SRGB, data=small), ts_ms
        )
        # Le modèle selfie ne renvoie qu'un masque (personne) ; les modèles
        # multi-classes renvoient le fond en premier.
        masks = res.confidence_masks
        m = masks[0].numpy_view().reshape(sh, sw)
        if len(masks) > 1:
            m = 1.0 - m
        # Resserre la transition, puis lisse dans le temps contre le scintillement.
        m = np.clip((m - 0.35) / 0.3, 0.0, 1.0).astype(np.float32)
        if self.prev is not None and self.prev.shape == m.shape:
            m = cv2.addWeighted(m, 0.65, self.prev, 0.35, 0)
        self.prev = m
        m = cv2.resize(m, (w, h), interpolation=cv2.INTER_LINEAR)
        return cv2.GaussianBlur(m, (7, 7), 0)


def composite(frame: np.ndarray, bg: np.ndarray, mask: np.ndarray) -> np.ndarray:
    return cv2.blendLinear(frame, bg, mask, 1.0 - mask)


# --------------------------------------------------------------------------
# Gestes


class GestureWatcher(threading.Thread):
    """Reconnaît les gestes dans un fil séparé, sur la trame la plus récente,
    pour ne pas ralentir la boucle vidéo. `poll()` rend une action au plus
    une fois par geste tenu."""

    def __init__(self, cfg: Config):
        super().__init__(daemon=True)
        self.cfg = cfg
        self.rec = vision.GestureRecognizer.create_from_options(
            vision.GestureRecognizerOptions(
                base_options=mp_tasks.BaseOptions(
                    model_asset_path=model_path("gesture_recognizer.task")
                ),
                running_mode=vision.RunningMode.VIDEO,
                num_hands=1,
            )
        )
        self.cond = threading.Condition()
        self.frame: np.ndarray | None = None
        self.ts = 0
        self.action: str | None = None
        self.candidate: str | None = None
        self.candidate_since = 0.0
        self.fired = False
        self.quiet_until = 0.0
        self.last_seen: str | None = None

    def submit(self, frame: np.ndarray, ts_ms: int) -> None:
        with self.cond:
            self.frame, self.ts = frame, ts_ms
            self.cond.notify()

    def poll(self) -> str | None:
        with self.cond:
            action, self.action = self.action, None
            return action

    def run(self) -> None:
        while True:
            with self.cond:
                while self.frame is None:
                    self.cond.wait()
                frame, ts = self.frame, self.ts
                self.frame = None
            small = cv2.resize(frame, (640, round(frame.shape[0] * 640 / frame.shape[1])))
            rgb = cv2.cvtColor(small, cv2.COLOR_BGR2RGB)
            res = self.rec.recognize_for_video(
                mp.Image(image_format=mp.ImageFormat.SRGB, data=rgb), ts
            )
            name = None
            if res.gestures:
                top = res.gestures[0][0]
                if top.category_name != "None" and top.score >= self.cfg.min_score:
                    name = top.category_name
            self.observe(name, time.monotonic())

    def observe(self, name: str | None, now: float) -> None:
        self.last_seen = name
        if name != self.candidate:
            self.candidate, self.candidate_since, self.fired = name, now, False
            return
        if (
            name is None
            or self.fired
            or now < self.quiet_until
            or now - self.candidate_since < self.cfg.hold
        ):
            return
        self.fired = True
        self.quiet_until = now + self.cfg.cooldown
        action = self.cfg.gesture_map.get(name)
        print(f"bgcam: geste {name} -> {action or '(non associé)'}", file=sys.stderr)
        if action:
            with self.cond:
                self.action = action


# --------------------------------------------------------------------------
# Entrée / sortie


def open_camera(cfg: Config) -> cv2.VideoCapture:
    cap = cv2.VideoCapture(cfg.cam_device, cv2.CAP_V4L2)
    if not cap.isOpened():
        raise SystemExit(
            f"bgcam: impossible d'ouvrir {cfg.cam_device} "
            "(déjà utilisée par une autre application ?)"
        )
    # MJPG : seul format qui tient 30 i/s en 720p et plus sur la plupart des webcams.
    cap.set(cv2.CAP_PROP_FOURCC, cv2.VideoWriter_fourcc(*"MJPG"))
    cap.set(cv2.CAP_PROP_FRAME_WIDTH, cfg.width)
    cap.set(cv2.CAP_PROP_FRAME_HEIGHT, cfg.height)
    cap.set(cv2.CAP_PROP_FPS, cfg.fps)
    cap.set(cv2.CAP_PROP_BUFFERSIZE, 1)
    return cap


def open_output(cfg: Config):
    import pyvirtualcam

    if not Path(cfg.out_device).exists():
        raise SystemExit(
            f"bgcam: {cfg.out_device} n'existe pas. Créer la webcam virtuelle :\n"
            "  sudo modprobe v4l2loopback devices=1 video_nr=10 "
            'card_label="bgcam" exclusive_caps=1\n'
            "ou lancer avec --preview / --no-output pour tester sans."
        )
    return pyvirtualcam.Camera(
        cfg.width,
        cfg.height,
        cfg.fps,
        device=cfg.out_device,
        fmt=pyvirtualcam.PixelFormat.BGR,
    )


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--config", type=Path, help=f"défaut : {DEFAULT_CONFIG}")
    ap.add_argument("--bg", help="fond de départ (blur, none, ou nom configuré)")
    ap.add_argument("--preview", action="store_true", help="affiche une fenêtre")
    ap.add_argument(
        "--no-output", action="store_true", help="n'écrit pas sur la webcam virtuelle"
    )
    ap.add_argument(
        "--input", help="image ou vidéo à traiter à la place de la webcam (test)"
    )
    ap.add_argument("--snapshot", type=Path, help="écrit la dernière trame ici (test)")
    ap.add_argument("--frames", type=int, help="s'arrête après N trames (test)")
    args = ap.parse_args()

    cfg = load_config(args.config)
    still = None
    if args.input:
        still = cv2.imread(args.input)
        cap = None if still is not None else cv2.VideoCapture(args.input)
        if still is None and not cap.isOpened():
            raise SystemExit(f"bgcam: entrée illisible : {args.input}")
    else:
        cap = open_camera(cfg)

    backgrounds = Backgrounds(cfg)
    if args.bg:
        backgrounds.apply(args.bg)
    segmenter = Segmenter()
    watcher = None
    if cfg.gestures_enabled and cfg.gesture_map:
        watcher = GestureWatcher(cfg)
        watcher.start()
    out = None if args.no_output else open_output(cfg)
    if out:
        print(f"bgcam: sortie sur {out.device}", file=sys.stderr)

    start = time.monotonic()
    n, last_ts, result = 0, -1, None
    try:
        while args.frames is None or n < args.frames:
            if still is not None:
                frame = still
                time.sleep(1 / cfg.fps)
            else:
                ok, frame = cap.read()
                if not ok:
                    if args.input:
                        break
                    raise SystemExit("bgcam: lecture de la webcam interrompue")
            if frame.shape[1::-1] != (cfg.width, cfg.height):
                frame = cover(frame, cfg.width, cfg.height)
            ts = max(last_ts + 1, int((time.monotonic() - start) * 1000))
            last_ts = ts

            if watcher:
                watcher.submit(frame, ts)
                action = watcher.poll()
                if action:
                    backgrounds.apply(action)

            bg = backgrounds.render(frame)
            result = frame if bg is None else composite(frame, bg, segmenter.mask(frame, ts))
            n += 1

            if out:
                out.send(result)
            if args.preview:
                cv2.imshow("bgcam", result)
                if cv2.waitKey(1) & 0xFF in (27, ord("q")):
                    break
            if n % 300 == 0:
                fps = n / (time.monotonic() - start)
                print(f"bgcam: {fps:.1f} i/s, fond {backgrounds.current}", file=sys.stderr)
    except KeyboardInterrupt:
        pass
    finally:
        if cap is not None:
            cap.release()
        if out:
            out.close()
        if args.snapshot and result is not None:
            cv2.imwrite(str(args.snapshot), result)
        if n:
            dt = time.monotonic() - start
            print(f"bgcam: {n} trames en {dt:.1f} s ({n / dt:.1f} i/s)", file=sys.stderr)


if __name__ == "__main__":
    main()
