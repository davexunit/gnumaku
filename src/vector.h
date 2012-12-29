#ifndef GNUMAKU_VECTOR2_H
#define GNUMAKU_VECTOR2_H

#include <allegro5/allegro.h>
#include "libguile.h"

typedef struct {
    float x, y;
} Vector2;

bool vector2_equal (Vector2 a, Vector2 b);
float vector2_mag (Vector2 v);
float vector2_dot (Vector2 a, Vector2 b);
float vector2_cross (Vector2 a, Vector2 b);
float vector2_angle (Vector2 v);
Vector2 vector2_zero ();
Vector2 vector2_new (float x, float y);
Vector2 vector2_add (Vector2 a, Vector2 b);
Vector2 vector2_sub (Vector2 a, Vector2 b);
Vector2 vector2_scale (Vector2 v, float scalar);
Vector2 vector2_norm (Vector2 v);
Vector2 vector2_from_polar (float radius, float angle);
Vector2 vector2_right_normal (Vector2 v);
Vector2 vector2_left_normal (Vector2 v);
void init_vector2_type (void);

#endif
