/* Fenêtre de réglage : SDL2 pour l'affichage et les événements, SDL2_ttf
   pour le texte. Une seule fenêtre par processus ; tout le dessin des
   commandes est fait en OCaml dans un tampon RGB. */

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <string.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

static SDL_Window *win;
static SDL_Renderer *ren;
static SDL_Texture *tex;
static TTF_Font *font;
static int win_w, win_h;

static const char *fonts[] = {
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    NULL};

value bgcam_ui_open(value title, value w, value h, value size) {
  CAMLparam4(title, w, h, size);
  win_w = Int_val(w);
  win_h = Int_val(h);
  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    caml_failwith(SDL_GetError());
  if (TTF_Init() != 0)
    caml_failwith(TTF_GetError());
  for (int i = 0; fonts[i] && !font; i++)
    font = TTF_OpenFont(fonts[i], Int_val(size));
  if (!font)
    caml_failwith("aucune police trouvée (DejaVu ou Liberation)");
  win = SDL_CreateWindow(String_val(title), SDL_WINDOWPOS_CENTERED,
                         SDL_WINDOWPOS_CENTERED, win_w, win_h,
                         SDL_WINDOW_RESIZABLE);
  if (!win)
    caml_failwith(SDL_GetError());
  ren = SDL_CreateRenderer(win, -1, 0);
  if (!ren)
    caml_failwith(SDL_GetError());
  tex = SDL_CreateTexture(ren, SDL_PIXELFORMAT_RGB24,
                          SDL_TEXTUREACCESS_STREAMING, win_w, win_h);
  if (!tex)
    caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

value bgcam_ui_min_size(value w, value h) {
  SDL_SetWindowMinimumSize(win, Int_val(w), Int_val(h));
  return Val_unit;
}

/* Affiche le tampon RGB de [w] x [h] ; la texture suit sa taille quand la
   fenêtre est redimensionnée. */
value bgcam_ui_present(value buf, value w, value h) {
  CAMLparam3(buf, w, h);
  void *p = Caml_ba_data_val(buf);
  if (Int_val(w) != win_w || Int_val(h) != win_h) {
    SDL_Texture *t =
        SDL_CreateTexture(ren, SDL_PIXELFORMAT_RGB24,
                          SDL_TEXTUREACCESS_STREAMING, Int_val(w), Int_val(h));
    if (t) {
      SDL_DestroyTexture(tex);
      tex = t;
      win_w = Int_val(w);
      win_h = Int_val(h);
    }
  }
  caml_release_runtime_system();
  SDL_UpdateTexture(tex, NULL, p, win_w * 3);
  SDL_RenderCopy(ren, tex, NULL, NULL);
  SDL_RenderPresent(ren);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

/* Écrit [s] dans le tampon RGB (largeur bw, hauteur bh) en (x, y), avec
   mélange alpha. Renvoie la largeur du texte. */
value bgcam_ui_text(value buf, value dims, value pos, value s, value rgb) {
  CAMLparam5(buf, dims, pos, s, rgb);
  int bw = Int_val(Field(dims, 0)), bh = Int_val(Field(dims, 1));
  int x0 = Int_val(Field(pos, 0)), y0 = Int_val(Field(pos, 1));
  int c = Int_val(rgb);
  if (caml_string_length(s) == 0)
    CAMLreturn(Val_int(0));
  SDL_Color white = {255, 255, 255, 255};
  SDL_Surface *t = TTF_RenderUTF8_Blended(font, String_val(s), white);
  if (!t)
    CAMLreturn(Val_int(0));
  SDL_Surface *a = SDL_ConvertSurfaceFormat(t, SDL_PIXELFORMAT_ARGB8888, 0);
  SDL_FreeSurface(t);
  if (!a)
    CAMLreturn(Val_int(0));
  unsigned char *dst = Caml_ba_data_val(buf);
  int cr = (c >> 16) & 255, cg = (c >> 8) & 255, cb = c & 255;
  for (int y = 0; y < a->h; y++) {
    int yy = y0 + y;
    if (yy < 0 || yy >= bh)
      continue;
    Uint32 *row = (Uint32 *)((unsigned char *)a->pixels + y * a->pitch);
    for (int x = 0; x < a->w; x++) {
      int xx = x0 + x;
      if (xx < 0 || xx >= bw)
        continue;
      int al = row[x] >> 24;
      if (!al)
        continue;
      unsigned char *d = dst + (yy * bw + xx) * 3;
      d[0] = (d[0] * (255 - al) + cr * al) / 255;
      d[1] = (d[1] * (255 - al) + cg * al) / 255;
      d[2] = (d[2] * (255 - al) + cb * al) / 255;
    }
  }
  int w = a->w;
  SDL_FreeSurface(a);
  CAMLreturn(Val_int(w));
}

/* Prochain événement : (genre, x, y). Genre : 0 aucun, 1 fermeture,
   2 bouton enfoncé, 3 bouton relâché, 4 mouvement, 5 molette vers le haut,
   6 molette vers le bas, 7 fenêtre redimensionnée (x, y = nouvelle taille). */
value bgcam_ui_poll(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(r);
  SDL_Event e;
  int kind = 0, x = 0, y = 0;
  while (kind == 0 && SDL_PollEvent(&e)) {
    switch (e.type) {
    case SDL_QUIT:
      kind = 1;
      break;
    case SDL_MOUSEBUTTONDOWN:
      if (e.button.button == SDL_BUTTON_LEFT)
        kind = 2, x = e.button.x, y = e.button.y;
      break;
    case SDL_MOUSEBUTTONUP:
      if (e.button.button == SDL_BUTTON_LEFT)
        kind = 3, x = e.button.x, y = e.button.y;
      break;
    case SDL_MOUSEMOTION:
      kind = 4, x = e.motion.x, y = e.motion.y;
      break;
    case SDL_WINDOWEVENT:
      if (e.window.event == SDL_WINDOWEVENT_SIZE_CHANGED)
        kind = 7, x = e.window.data1, y = e.window.data2;
      break;
    case SDL_MOUSEWHEEL:
      if (e.wheel.y != 0) {
        SDL_GetMouseState(&x, &y);
        kind = e.wheel.y > 0 ? 5 : 6;
      }
      break;
    }
  }
  r = caml_alloc_tuple(3);
  Store_field(r, 0, Val_int(kind));
  Store_field(r, 1, Val_int(x));
  Store_field(r, 2, Val_int(y));
  CAMLreturn(r);
}

value bgcam_ui_close(value unit) {
  if (ren)
    SDL_DestroyRenderer(ren);
  if (win)
    SDL_DestroyWindow(win);
  win = NULL;
  ren = NULL;
  SDL_Quit();
  return Val_unit;
}
