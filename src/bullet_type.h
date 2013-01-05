#ifndef GMK_BULLET_TYPE_H
#define GMK_BULLET_TYPE_H

#include "common.h"
#include "blend_mode.h"
#include "rect.h"

/*
 * BulletType
 *
 * Common set of properties for bullets.
 */
typedef struct {
    bool directional;
    int image;
    Rect hitbox;
    GmkBlendMode blend_mode;
} GmkBulletType;

GmkBulletType *gmk_check_bullet_type (SCM bullet_type_smob);

SCM gmk_make_bullet_type (SCM s_image, SCM s_hitbox, SCM s_blend_mode,
                          SCM s_directional);

void gmk_init_bullet_type (void);

#endif
