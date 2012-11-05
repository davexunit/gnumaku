#ifndef SPRITE_H
#define SPRITE_H

#include <allegro5/allegro.h>

#include "sprite_sheet.h"

typedef struct
{
    SCM sprite_sheet_smob;
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
    float x, y;
    float center_x, center_y;
    float scale_x, scale_y;
    float rotation;
    bool visible;
} Sprite;

void init_sprite_type (void);

#endif
