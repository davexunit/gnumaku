#ifndef GMK_BLEND_MODE_H
#define GMK_BLEND_MODE_H

#include <allegro5/allegro.h>
#include <libguile.h>

typedef enum {
    GMK_BLEND_ALPHA,
    GMK_BLEND_ADD,
} GmkBlendMode;

GmkBlendMode gmk_scm_to_blend_mode (SCM blend_mode);

void gmk_set_blend_mode (GmkBlendMode blend_mode);

void init_blend_mode (void);

#endif
