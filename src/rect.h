#ifndef GMK_RECT_H
#define GMK_RECT_H

#include <libguile.h>
#include "vector.h"

typedef struct {
    float x, y;
    float width, height;
} GmkRect;

GmkRect gmk_rect_new (float x, float y, float width, float height);
GmkRect gmk_rect_move (GmkRect rect, GmkVector2 delta);
GmkRect gmk_rect_scale (GmkRect rect, GmkVector2 scale);
GmkRect gmk_rect_center (GmkRect rect, GmkVector2 center);
GmkVector2 gmk_get_rect_center (GmkRect rect);
bool gmk_rect_collide_point (GmkRect rect, GmkVector2 p);
bool gmk_rect_collide_rect (GmkRect rect, GmkRect other);

/* Scheme bindings. */
GmkRect gmk_scm_to_rect (SCM rect_smob);
SCM gmk_scm_from_rect (GmkRect rect);
SCM gmk_s_make_rect (SCM x, SCM y, SCM width, SCM height);
SCM gmk_s_rect_x (SCM rect);
SCM gmk_s_rect_y (SCM rect);
SCM gmk_s_rect_width (SCM rect);
SCM gmk_s_rect_height (SCM rect);
SCM gmk_s_rect_move (SCM rect, SCM delta);
SCM gmk_s_rect_center (SCM rect, SCM center);
SCM gmk_s_rect_collide_point (SCM rect, SCM point);
SCM gmk_s_rect_collide_rect (SCM rect1, SCM rect2);
void gmk_init_rect (void);

#endif
