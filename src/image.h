#ifndef GMK_IMAGE_H
#define GMK_IMAGE_H

#include "common.h"

/* A wrapper for ALLEGRO_BITMAP */
typedef struct {
    ALLEGRO_BITMAP *bitmap;
} GmkImage;

ALLEGRO_BITMAP *gmk_scm_to_bitmap (SCM image);
SCM gmk_scm_from_bitmap (ALLEGRO_BITMAP *bitmap);
SCM gmk_load_image (SCM filename);
SCM gmk_image_width (SCM image);
SCM gmk_image_height (SCM image);
SCM gmk_draw_image(SCM image, SCM x, SCM y);
SCM gmk_set_render_image (SCM image);

void gmk_init_image (void);

#endif
