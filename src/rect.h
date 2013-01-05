#ifndef RECT_H
#define RECT_H

#include <allegro5/allegro.h>
#include <libguile.h>
#include "vector.h"

typedef struct {
    float x, y;
    float width, height;
} Rect;

Rect rect_new (float x, float y, float width, float height);
bool rect_collide_point (Rect rect, GmkVector2 p);
bool rect_collide_rect (Rect rect, Rect other);
Rect rect_move (Rect rect, GmkVector2 delta);
Rect rect_scale (Rect rect, GmkVector2 scale);
GmkVector2 get_rect_center (Rect rect);
Rect rect_center (Rect rect, GmkVector2 center);
SCM make_rect (SCM s_x, SCM s_y, SCM s_width, SCM s_height);
Rect* check_rect (SCM rect_smob);
Rect scm_to_rect (SCM rect_smob);
SCM scm_from_rect (Rect rect);
void init_rect_type (void);

#endif
