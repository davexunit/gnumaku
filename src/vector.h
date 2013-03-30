#ifndef GMK_VECTOR2_H
#define GMK_VECTOR2_H

#include <allegro5/allegro.h>

typedef struct {
    float x, y;
} GmkVector2;

bool gmk_vector2_equal (GmkVector2 a, GmkVector2 b);
float gmk_vector2_mag (GmkVector2 v);
float gmk_vector2_dot (GmkVector2 a, GmkVector2 b);
float gmk_vector2_cross (GmkVector2 a, GmkVector2 b);
float gmk_vector2_angle (GmkVector2 v);
GmkVector2 gmk_vector2_zero ();
GmkVector2 gmk_vector2_new (float x, float y);
GmkVector2 gmk_vector2_add (GmkVector2 a, GmkVector2 b);
GmkVector2 gmk_vector2_sub (GmkVector2 a, GmkVector2 b);
GmkVector2 gmk_vector2_scale (GmkVector2 v, float scalar);
GmkVector2 gmk_vector2_norm (GmkVector2 v);
GmkVector2 gmk_vector2_from_polar (float radius, float angle);
GmkVector2 gmk_vector2_right_normal (GmkVector2 v);
GmkVector2 gmk_vector2_left_normal (GmkVector2 v);

#endif
