#ifndef GMK_SPRITE_H
#define GMK_SPRITE_H

#include "common.h"
#include "color.h"
#include "image.h"

typedef struct {
    SCM image;
    ALLEGRO_COLOR color;
    Vector2 position;
    Vector2 scale;
    Vector2 anchor;
    float rotation;
} GmkSprite;

GmkSprite gmk_scm_to_sprite (SCM sprite);
SCM gmk_scm_from_sprite (GmkSprite sprite);
SCM gmk_make_sprite (SCM image, SCM kw_args);
SCM gmk_sprite_image (SCM sprite);
SCM gmk_sprite_position (SCM sprite);
SCM gmk_sprite_scale (SCM sprite);
SCM gmk_sprite_rotation (SCM sprite);
SCM gmk_sprite_color (SCM sprite);
SCM gmk_sprite_opacity (SCM sprite);
SCM gmk_sprite_anchor (SCM sprite);
SCM gmk_sprite_image (SCM sprite);
SCM gmk_draw_sprite (SCM sprite);
void gmk_init_sprite (void);

#endif
