/* Frontière C de bgcam : uniquement ce qui parle à une bibliothèque système
   (TFLite, V4L2, libjpeg, libwebp). Tout le traitement d'image est en OCaml. */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

#include <jpeglib.h>
#include <linux/videodev2.h>
#include <tensorflow/lite/c/builtin_op_data.h>
#include <tensorflow/lite/c/c_api.h>
#include <tensorflow/lite/c/c_api_experimental.h>
#include <tensorflow/lite/c/common.h>
#include <webp/demux.h>

#define Ptr_val(v) (*((void **)Data_abstract_val(v)))

static value alloc_ptr(void *p) {
  value v = caml_alloc(1, Abstract_tag);
  Ptr_val(v) = p;
  return v;
}

static void failf(const char *what, const char *detail) {
  char msg[512];
  snprintf(msg, sizeof msg, "%s: %s", what, detail);
  caml_failwith(msg);
}

/* ------------------------------------------------------------------------
   TFLite */

/* Convolution2DTransposeBias : opération propre à MediaPipe, utilisée par le
   modèle selfie_segmenter et absente de la bibliothèque TFLite standard.
   Entrées : image NHWC, poids OHWI, biais. Paramètres : padding, pas. */

static void tcb_geometry(TfLiteContext *ctx, TfLiteNode *node, int *oh, int *ow,
                         int *pad_h, int *pad_w, int *sh, int *sw) {
  const TfLiteTensor *in = &ctx->tensors[node->inputs->data[0]];
  const TfLiteTensor *w = &ctx->tensors[node->inputs->data[1]];
  const int32_t *p = (const int32_t *)node->custom_initial_data;
  int same = p[0] == kTfLitePaddingSame;
  *sw = p[1];
  *sh = p[2];
  int ih = in->dims->data[1], iw = in->dims->data[2];
  int kh = w->dims->data[1], kw = w->dims->data[2];
  *oh = same ? ih * *sh : (ih - 1) * *sh + kh;
  *ow = same ? iw * *sw : (iw - 1) * *sw + kw;
  int eh = (ih - 1) * *sh + kh - *oh, ew = (iw - 1) * *sw + kw - *ow;
  *pad_h = eh > 0 ? eh / 2 : 0;
  *pad_w = ew > 0 ? ew / 2 : 0;
}

static TfLiteStatus tcb_prepare(TfLiteContext *ctx, TfLiteNode *node) {
  if (node->inputs->size != 3 || node->custom_initial_data_size < 12)
    return kTfLiteError;
  const TfLiteTensor *in = &ctx->tensors[node->inputs->data[0]];
  const TfLiteTensor *w = &ctx->tensors[node->inputs->data[1]];
  TfLiteTensor *out = &ctx->tensors[node->outputs->data[0]];
  int oh, ow, ph, pw, sh, sw;
  tcb_geometry(ctx, node, &oh, &ow, &ph, &pw, &sh, &sw);
  TfLiteIntArray *dims = TfLiteIntArrayCreate(4);
  dims->data[0] = in->dims->data[0];
  dims->data[1] = oh;
  dims->data[2] = ow;
  dims->data[3] = w->dims->data[0];
  return ctx->ResizeTensor(ctx, out, dims);
}

static TfLiteStatus tcb_eval(TfLiteContext *ctx, TfLiteNode *node) {
  const TfLiteTensor *in = &ctx->tensors[node->inputs->data[0]];
  const TfLiteTensor *wt = &ctx->tensors[node->inputs->data[1]];
  const TfLiteTensor *bt = &ctx->tensors[node->inputs->data[2]];
  TfLiteTensor *out = &ctx->tensors[node->outputs->data[0]];
  int oh, ow, ph, pw, sh, sw;
  tcb_geometry(ctx, node, &oh, &ow, &ph, &pw, &sh, &sw);
  int nb = in->dims->data[0], ih = in->dims->data[1], iw = in->dims->data[2];
  int ic = in->dims->data[3];
  int oc = wt->dims->data[0], kh = wt->dims->data[1], kw = wt->dims->data[2];
  const float *w = wt->data.f, *bias = bt->data.f;

  for (int b = 0; b < nb; b++) {
    const float *ib = in->data.f + (size_t)b * ih * iw * ic;
    float *ob = out->data.f + (size_t)b * oh * ow * oc;
    for (size_t i = 0; i < (size_t)oh * ow; i++)
      memcpy(ob + i * oc, bias, oc * sizeof(float));
    for (int y = 0; y < ih; y++)
      for (int ky = 0; ky < kh; ky++) {
        int oy = y * sh + ky - ph;
        if (oy < 0 || oy >= oh) continue;
        for (int x = 0; x < iw; x++) {
          const float *ip = ib + ((size_t)y * iw + x) * ic;
          for (int kx = 0; kx < kw; kx++) {
            int ox = x * sw + kx - pw;
            if (ox < 0 || ox >= ow) continue;
            float *op = ob + ((size_t)oy * ow + ox) * oc;
            for (int o = 0; o < oc; o++) {
              const float *wp = w + (((size_t)o * kh + ky) * kw + kx) * ic;
              float acc = 0;
              for (int i = 0; i < ic; i++) acc += ip[i] * wp[i];
              op[o] += acc;
            }
          }
        }
      }
  }
  return kTfLiteOk;
}

static TfLiteRegistration tcb_registration = {
    .prepare = tcb_prepare,
    .invoke = tcb_eval,
};

static void quiet_reporter(void *user, const char *fmt, va_list args) {
  (void)user;
  if (strstr(fmt, "XNNPACK")) return;
  vfprintf(stderr, fmt, args);
  fputc('\n', stderr);
}

CAMLprim value bgcam_tfl_load(value path, value threads) {
  CAMLparam2(path, threads);
  TfLiteModel *model = TfLiteModelCreateFromFile(String_val(path));
  if (!model) failf("modèle TFLite illisible", String_val(path));
  TfLiteInterpreterOptions *opts = TfLiteInterpreterOptionsCreate();
  TfLiteInterpreterOptionsSetNumThreads(opts, Int_val(threads));
  TfLiteInterpreterOptionsSetErrorReporter(opts, quiet_reporter, NULL);
  TfLiteInterpreterOptionsAddCustomOp(opts, "Convolution2DTransposeBias",
                                      &tcb_registration, 1, 1);
  TfLiteInterpreter *it = TfLiteInterpreterCreate(model, opts);
  if (!it || TfLiteInterpreterAllocateTensors(it) != kTfLiteOk)
    failf("modèle TFLite non chargé", String_val(path));
  CAMLreturn(alloc_ptr(it));
}

CAMLprim value bgcam_tfl_output_len(value m, value idx) {
  const TfLiteTensor *t =
      TfLiteInterpreterGetOutputTensor(Ptr_val(m), Int_val(idx));
  if (!t || TfLiteTensorType(t) != kTfLiteFloat32)
    caml_failwith("TFLite: sortie absente ou non flottante");
  return Val_long(TfLiteTensorByteSize(t) / sizeof(float));
}

CAMLprim value bgcam_tfl_input_name(value m, value idx) {
  CAMLparam2(m, idx);
  TfLiteTensor *t = TfLiteInterpreterGetInputTensor(Ptr_val(m), Int_val(idx));
  if (!t) caml_failwith("TFLite: entrée absente");
  CAMLreturn(caml_copy_string(TfLiteTensorName(t)));
}

CAMLprim value bgcam_tfl_set_input(value m, value idx, value ba) {
  TfLiteTensor *t = TfLiteInterpreterGetInputTensor(Ptr_val(m), Int_val(idx));
  size_t n = caml_ba_byte_size(Caml_ba_array_val(ba));
  if (!t || TfLiteTensorCopyFromBuffer(t, Caml_ba_data_val(ba), n) != kTfLiteOk)
    caml_failwith("TFLite: taille d'entrée inattendue");
  return Val_unit;
}

CAMLprim value bgcam_tfl_get_output(value m, value idx, value ba) {
  const TfLiteTensor *t =
      TfLiteInterpreterGetOutputTensor(Ptr_val(m), Int_val(idx));
  size_t n = caml_ba_byte_size(Caml_ba_array_val(ba));
  if (!t || TfLiteTensorCopyToBuffer(t, Caml_ba_data_val(ba), n) != kTfLiteOk)
    caml_failwith("TFLite: taille de sortie inattendue");
  return Val_unit;
}

CAMLprim value bgcam_tfl_invoke(value m) {
  CAMLparam1(m);
  TfLiteInterpreter *it = Ptr_val(m);
  caml_release_runtime_system();
  TfLiteStatus st = TfLiteInterpreterInvoke(it);
  caml_acquire_runtime_system();
  if (st != kTfLiteOk) caml_failwith("TFLite: échec de l'inférence");
  CAMLreturn(Val_unit);
}

/* ------------------------------------------------------------------------
   JPEG */

struct jerr {
  struct jpeg_error_mgr pub;
  jmp_buf jmp;
};

static void jerr_exit(j_common_ptr cinfo) {
  longjmp(((struct jerr *)cinfo->err)->jmp, 1);
}

static void jerr_silent(j_common_ptr cinfo) { (void)cinfo; }

/* Décode vers RGB24. Si `dst` est NULL, ne lit que les dimensions.
   Rend 0 en cas de succès. */
static int jpeg_to_rgb(const uint8_t *src, size_t len, uint8_t *dst, int *w,
                       int *h) {
  struct jpeg_decompress_struct c;
  struct jerr err;
  c.err = jpeg_std_error(&err.pub);
  err.pub.error_exit = jerr_exit;
  err.pub.output_message = jerr_silent;
  if (setjmp(err.jmp)) {
    jpeg_destroy_decompress(&c);
    return -1;
  }
  jpeg_create_decompress(&c);
  jpeg_mem_src(&c, src, len);
  jpeg_read_header(&c, TRUE);
  c.out_color_space = JCS_RGB;
  c.dct_method = JDCT_IFAST;
  jpeg_calc_output_dimensions(&c);
  if (!dst) {
    *w = c.output_width;
    *h = c.output_height;
    jpeg_destroy_decompress(&c);
    return 0;
  }
  if ((int)c.output_width != *w || (int)c.output_height != *h) {
    jpeg_destroy_decompress(&c);
    return -1;
  }
  jpeg_start_decompress(&c);
  while (c.output_scanline < c.output_height) {
    JSAMPROW row = dst + (size_t)c.output_scanline * c.output_width * 3;
    jpeg_read_scanlines(&c, &row, 1);
  }
  jpeg_finish_decompress(&c);
  jpeg_destroy_decompress(&c);
  return 0;
}

CAMLprim value bgcam_jpeg_size(value data) {
  CAMLparam1(data);
  CAMLlocal1(res);
  int w = 0, h = 0;
  if (jpeg_to_rgb((const uint8_t *)String_val(data), caml_string_length(data),
                  NULL, &w, &h))
    caml_failwith("JPEG illisible");
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_int(w));
  Store_field(res, 1, Val_int(h));
  CAMLreturn(res);
}

CAMLprim value bgcam_jpeg_decode(value data, value ba, value w, value h) {
  int iw = Int_val(w), ih = Int_val(h);
  if (jpeg_to_rgb((const uint8_t *)String_val(data), caml_string_length(data),
                  Caml_ba_data_val(ba), &iw, &ih))
    caml_failwith("JPEG illisible");
  return Val_unit;
}

CAMLprim value bgcam_jpeg_write(value path, value ba, value w, value h) {
  CAMLparam4(path, ba, w, h);
  FILE *f = fopen(String_val(path), "wb");
  if (!f) failf("écriture impossible", String_val(path));
  struct jpeg_compress_struct c;
  struct jpeg_error_mgr err;
  c.err = jpeg_std_error(&err);
  jpeg_create_compress(&c);
  jpeg_stdio_dest(&c, f);
  c.image_width = Int_val(w);
  c.image_height = Int_val(h);
  c.input_components = 3;
  c.in_color_space = JCS_RGB;
  jpeg_set_defaults(&c);
  jpeg_set_quality(&c, 92, TRUE);
  jpeg_start_compress(&c, TRUE);
  uint8_t *px = Caml_ba_data_val(ba);
  while (c.next_scanline < c.image_height) {
    JSAMPROW row = px + (size_t)c.next_scanline * c.image_width * 3;
    jpeg_write_scanlines(&c, &row, 1);
  }
  jpeg_finish_compress(&c);
  jpeg_destroy_compress(&c);
  fclose(f);
  CAMLreturn(Val_unit);
}

/* ------------------------------------------------------------------------
   WebP animé */

struct webp {
  uint8_t *bytes;
  WebPAnimDecoder *dec;
  int w, h;
};

CAMLprim value bgcam_webp_open(value data) {
  CAMLparam1(data);
  CAMLlocal1(res);
  struct webp *s = calloc(1, sizeof *s);
  size_t len = caml_string_length(data);
  s->bytes = malloc(len);
  memcpy(s->bytes, String_val(data), len);
  WebPData wd = {s->bytes, len};
  WebPAnimDecoderOptions opt;
  WebPAnimDecoderOptionsInit(&opt);
  opt.color_mode = MODE_RGBA;
  opt.use_threads = 1;
  s->dec = WebPAnimDecoderNew(&wd, &opt);
  WebPAnimInfo info;
  if (!s->dec || !WebPAnimDecoderGetInfo(s->dec, &info)) {
    free(s->bytes);
    free(s);
    caml_failwith("WebP illisible");
  }
  s->w = info.canvas_width;
  s->h = info.canvas_height;
  res = caml_alloc_tuple(3);
  Store_field(res, 0, alloc_ptr(s));
  Store_field(res, 1, Val_int(s->w));
  Store_field(res, 2, Val_int(s->h));
  CAMLreturn(res);
}

/* Écrit la trame suivante en RGB24 (sur fond noir) et rend son horodatage de
   fin en ms, ou -1 quand il n'y en a plus (le décodeur est alors libéré). */
CAMLprim value bgcam_webp_next(value h, value ba) {
  struct webp *s = Ptr_val(h);
  if (!s) return Val_int(-1);
  uint8_t *rgba;
  int ts;
  if (!WebPAnimDecoderHasMoreFrames(s->dec) ||
      !WebPAnimDecoderGetNext(s->dec, &rgba, &ts)) {
    WebPAnimDecoderDelete(s->dec);
    free(s->bytes);
    free(s);
    Ptr_val(h) = NULL;
    return Val_int(-1);
  }
  uint8_t *dst = Caml_ba_data_val(ba);
  size_t n = (size_t)s->w * s->h;
  for (size_t i = 0; i < n; i++) {
    unsigned a = rgba[4 * i + 3];
    dst[3 * i] = rgba[4 * i] * a / 255;
    dst[3 * i + 1] = rgba[4 * i + 1] * a / 255;
    dst[3 * i + 2] = rgba[4 * i + 2] * a / 255;
  }
  return Val_int(ts);
}

/* ------------------------------------------------------------------------
   V4L2 : capture */

#define CAM_BUFS 4

struct cam {
  int fd, w, h;
  uint32_t fmt;
  void *start[CAM_BUFS];
  size_t len[CAM_BUFS];
};

static int xioctl(int fd, unsigned long req, void *arg) {
  int r;
  do r = ioctl(fd, req, arg);
  while (r == -1 && errno == EINTR);
  return r;
}

CAMLprim value bgcam_cam_open(value dev, value w, value h, value fps) {
  CAMLparam4(dev, w, h, fps);
  CAMLlocal1(res);
  int fd = open(String_val(dev), O_RDWR | O_CLOEXEC);
  if (fd < 0) failf(String_val(dev), strerror(errno));

  struct v4l2_format f = {.type = V4L2_BUF_TYPE_VIDEO_CAPTURE};
  f.fmt.pix.width = Int_val(w);
  f.fmt.pix.height = Int_val(h);
  f.fmt.pix.pixelformat = V4L2_PIX_FMT_MJPEG;
  f.fmt.pix.field = V4L2_FIELD_ANY;
  if (xioctl(fd, VIDIOC_S_FMT, &f) < 0) {
    int e = errno;
    close(fd);
    failf(String_val(dev), e == EBUSY
                               ? "webcam déjà utilisée par une autre application"
                               : strerror(e));
  }
  if (f.fmt.pix.pixelformat != V4L2_PIX_FMT_MJPEG &&
      f.fmt.pix.pixelformat != V4L2_PIX_FMT_YUYV) {
    close(fd);
    failf(String_val(dev), "ni MJPG ni YUYV proposés par la webcam");
  }
  struct v4l2_streamparm sp = {.type = V4L2_BUF_TYPE_VIDEO_CAPTURE};
  sp.parm.capture.timeperframe.numerator = 1;
  sp.parm.capture.timeperframe.denominator = Int_val(fps);
  xioctl(fd, VIDIOC_S_PARM, &sp);

  struct cam *c = calloc(1, sizeof *c);
  c->fd = fd;
  c->w = f.fmt.pix.width;
  c->h = f.fmt.pix.height;
  c->fmt = f.fmt.pix.pixelformat;

  struct v4l2_requestbuffers rb = {.count = CAM_BUFS,
                                   .type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
                                   .memory = V4L2_MEMORY_MMAP};
  if (xioctl(fd, VIDIOC_REQBUFS, &rb) < 0 || rb.count < 2) {
    int e = errno;
    close(fd);
    free(c);
    failf(String_val(dev), e == EBUSY
                               ? "webcam déjà utilisée par une autre application"
                               : "allocation des tampons refusée");
  }
  for (unsigned i = 0; i < rb.count && i < CAM_BUFS; i++) {
    struct v4l2_buffer b = {.type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
                            .memory = V4L2_MEMORY_MMAP,
                            .index = i};
    if (xioctl(fd, VIDIOC_QUERYBUF, &b) < 0)
      failf(String_val(dev), strerror(errno));
    c->len[i] = b.length;
    c->start[i] =
        mmap(NULL, b.length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, b.m.offset);
    if (c->start[i] == MAP_FAILED || xioctl(fd, VIDIOC_QBUF, &b) < 0)
      failf(String_val(dev), strerror(errno));
  }
  int type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  if (xioctl(fd, VIDIOC_STREAMON, &type) < 0)
    failf(String_val(dev), strerror(errno));

  res = caml_alloc_tuple(3);
  Store_field(res, 0, alloc_ptr(c));
  Store_field(res, 1, Val_int(c->w));
  Store_field(res, 2, Val_int(c->h));
  CAMLreturn(res);
}

static inline uint8_t clamp8(int v) { return v < 0 ? 0 : v > 255 ? 255 : v; }

static void yuyv_to_rgb(const uint8_t *src, uint8_t *dst, size_t pixels) {
  for (size_t i = 0; i < pixels / 2; i++) {
    int y0 = src[4 * i] - 16, u = src[4 * i + 1] - 128;
    int y1 = src[4 * i + 2] - 16, v = src[4 * i + 3] - 128;
    int r = 409 * v + 128, g = -100 * u - 208 * v + 128, b = 516 * u + 128;
    dst[6 * i] = clamp8((298 * y0 + r) >> 8);
    dst[6 * i + 1] = clamp8((298 * y0 + g) >> 8);
    dst[6 * i + 2] = clamp8((298 * y0 + b) >> 8);
    dst[6 * i + 3] = clamp8((298 * y1 + r) >> 8);
    dst[6 * i + 4] = clamp8((298 * y1 + g) >> 8);
    dst[6 * i + 5] = clamp8((298 * y1 + b) >> 8);
  }
}

/* Attend la trame suivante et la décode dans `ba` (RGB24, w*h*3).
   Rend false si la trame est corrompue (à ignorer). */
CAMLprim value bgcam_cam_read(value cam, value ba) {
  CAMLparam2(cam, ba);
  struct cam *c = Ptr_val(cam);
  uint8_t *dst = Caml_ba_data_val(ba);
  struct v4l2_buffer b = {.type = V4L2_BUF_TYPE_VIDEO_CAPTURE,
                          .memory = V4L2_MEMORY_MMAP};
  int ok = 0, err = 0;
  caml_release_runtime_system();
  if (xioctl(c->fd, VIDIOC_DQBUF, &b) < 0) {
    err = errno;
  } else {
    if (c->fmt == V4L2_PIX_FMT_MJPEG) {
      int w = c->w, h = c->h;
      ok = jpeg_to_rgb(c->start[b.index], b.bytesused, dst, &w, &h) == 0;
    } else if (b.bytesused >= (size_t)c->w * c->h * 2) {
      yuyv_to_rgb(c->start[b.index], dst, (size_t)c->w * c->h);
      ok = 1;
    }
    if (xioctl(c->fd, VIDIOC_QBUF, &b) < 0) err = errno;
  }
  caml_acquire_runtime_system();
  if (err) failf("lecture de la webcam", strerror(err));
  CAMLreturn(Val_bool(ok));
}

/* ------------------------------------------------------------------------
   V4L2 : sortie vers v4l2loopback */

struct out {
  int fd, w, h;
  uint8_t *yuyv;
};

CAMLprim value bgcam_out_open(value dev, value w, value h, value fps) {
  CAMLparam4(dev, w, h, fps);
  int fd = open(String_val(dev), O_RDWR | O_CLOEXEC);
  if (fd < 0) failf(String_val(dev), strerror(errno));
  struct out *o = calloc(1, sizeof *o);
  o->fd = fd;
  o->w = Int_val(w);
  o->h = Int_val(h);
  o->yuyv = malloc((size_t)o->w * o->h * 2);

  struct v4l2_format f = {.type = V4L2_BUF_TYPE_VIDEO_OUTPUT};
  f.fmt.pix.width = o->w;
  f.fmt.pix.height = o->h;
  f.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
  f.fmt.pix.field = V4L2_FIELD_NONE;
  f.fmt.pix.bytesperline = o->w * 2;
  f.fmt.pix.sizeimage = o->w * o->h * 2;
  f.fmt.pix.colorspace = V4L2_COLORSPACE_SRGB;
  if (xioctl(fd, VIDIOC_S_FMT, &f) < 0) {
    int e = errno;
    close(fd);
    failf(String_val(dev), e == ENOTTY || e == EINVAL
                               ? "ce n'est pas un périphérique v4l2loopback"
                               : strerror(e));
  }
  struct v4l2_streamparm sp = {.type = V4L2_BUF_TYPE_VIDEO_OUTPUT};
  sp.parm.output.timeperframe.numerator = 1;
  sp.parm.output.timeperframe.denominator = Int_val(fps);
  xioctl(fd, VIDIOC_S_PARM, &sp);
  CAMLreturn(alloc_ptr(o));
}

CAMLprim value bgcam_out_write(value out, value ba) {
  CAMLparam2(out, ba);
  struct out *o = Ptr_val(out);
  const uint8_t *rgb = Caml_ba_data_val(ba);
  size_t n = (size_t)o->w * o->h, len = n * 2;
  ssize_t written;
  caml_release_runtime_system();
  for (size_t i = 0; i < n / 2; i++) {
    const uint8_t *p = rgb + 6 * i;
    int r = (p[0] + p[3]) >> 1, g = (p[1] + p[4]) >> 1, b = (p[2] + p[5]) >> 1;
    o->yuyv[4 * i] = ((66 * p[0] + 129 * p[1] + 25 * p[2] + 128) >> 8) + 16;
    o->yuyv[4 * i + 1] = ((-38 * r - 74 * g + 112 * b + 128) >> 8) + 128;
    o->yuyv[4 * i + 2] = ((66 * p[3] + 129 * p[4] + 25 * p[5] + 128) >> 8) + 16;
    o->yuyv[4 * i + 3] = ((112 * r - 94 * g - 18 * b + 128) >> 8) + 128;
  }
  written = write(o->fd, o->yuyv, len);
  caml_acquire_runtime_system();
  if (written != (ssize_t)len) failf("écriture sur la webcam virtuelle", strerror(errno));
  CAMLreturn(Val_unit);
}
