#ifndef GMK_SPRITE_H
#define GMK_SPRITE_H

#include "common.h"
#include "color.h"
#include "image.h"

typedef struct {
    SCM image;
    ALLEGRO_COLOR color;
    GmkVector2 position;
    GmkVector2 scale;
    GmkVector2 anchor;
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
SCM gmk_set_sprite_image (SCM sprite, SCM image);
SCM gmk_set_sprite_position (SCM sprite, SCM position);
SCM gmk_set_sprite_scale (SCM sprite, SCM scale);
SCM gmk_set_sprite_rotation (SCM sprite, SCM rotation);
SCM gmk_set_sprite_color (SCM sprite, SCM color);
SCM gmk_set_sprite_opacity (SCM sprite, SCM opacity);
SCM gmk_set_sprite_anchor (SCM sprite, SCM anchor);
SCM gmk_set_draw_sprite (SCM sprite);
void gmk_init_sprite (void);

#endif
