#ifndef BULLET_TYPE_H
#define BULLET_TYPE_H

#include <allegro5/allegro.h>
#include <libguile.h>
#include "blend_mode.h"
#include "rect.h"

/*
 * BulletType
 *
 * Common properties for bullets of the same type.
 */
typedef struct {
    bool directional;
    int image;
    Rect hitbox;
    BlendMode blend_mode;
} BulletType;

BlendMode scm_to_blend_mode (SCM blend_mode);
BulletType* check_bullet_type (SCM bullet_type_smob);
/* Well, this name sure turned out silly. */
void init_bullet_type_type (void);

#endif
