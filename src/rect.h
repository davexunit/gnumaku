#ifndef RECT_H
#define RECT_H

#include <allegro5/allegro.h>

typedef struct
{
    float x, y;
    float width, height;
} Rect;

void init_rect (Rect *rect);
bool collide_point (Rect rect, float x, float y);
bool collide_rect (Rect rect, Rect other);

#endif
