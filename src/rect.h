#ifndef RECT_H
#define RECT_H

#include <allegro5/allegro.h>
#include <libguile.h>

typedef struct
{
    float x, y;
    float width, height;
} Rect;

Rect* check_rect (SCM rect_smob);
Rect scm_to_rect (SCM rect_smob);
void init_rect_type (void);
void init_rect (Rect *rect, float x, float y, float width, float height);
bool rect_collide_point (Rect *rect, float x, float y);
bool rect_collide_rect (Rect *rect, Rect *other);
Rect rect_move (Rect *rect, float dx, float dy);
SCM make_rect (SCM s_x, SCM s_y, SCM s_width, SCM s_height);

#endif
