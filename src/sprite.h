#ifndef SPRITE_H
#define SPRITE_H

#include <allegro5/allegro.h>

#include "image.h"

typedef struct
{
    SCM image;
    ALLEGRO_COLOR color;
    float x, y;
    float center_x, center_y;
    float scale_x, scale_y;
    float rotation;
    bool visible;
} Sprite;

void init_sprite_type (void);

#endif
