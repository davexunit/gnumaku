#ifndef SPRITE_H
#define SPRITE_H

#include <allegro5/allegro.h>

#include "color.h"
#include "image.h"
#include "vector.h"

typedef struct {
    SCM image;
    ALLEGRO_COLOR color;
    Vector2 position;
    Vector2 scale;
    Vector2 anchor;
    float rotation;
} Sprite;

void init_sprite_type (void);

#endif
