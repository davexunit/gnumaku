#include "rect.h"

void init_rect (Rect *rect)
{
    rect->x = 0;
    rect->y = 0;
    rect->width = 0;
    rect->height = 0;
}

bool collide_point (Rect rect, float x, float y)
{
    return x >= rect.x && x < rect.x + rect.width &&
	   y >= rect.y && y < rect.y + rect.height;
}

bool collide_rect (Rect rect, Rect other)
{
    return false;
}
