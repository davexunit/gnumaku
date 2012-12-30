#ifndef IMAGE_H
#define IMAGE_H

/* A wrapper for ALLEGRO_BITMAP */

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <libguile.h>

typedef struct {
    ALLEGRO_BITMAP *bitmap;
} Image;

int get_image_width (Image *image);
int get_image_height (Image *image);
void init_image_type ();
Image *check_image (SCM image_smob);
SCM make_image_from_bitmap (ALLEGRO_BITMAP *bitmap);

#endif
