#ifndef GMK_VECTOR2_H
#define GMK_VECTOR2_H

#include "common.h"

typedef struct {
    float x, y;
} GmkVector2;

/* C API. */
bool gmk_vector2_equal (GmkVector2 a, GmkVector2 b);
float gmk_vector2_mag (GmkVector2 v);
float gmk_vector2_dot (GmkVector2 a, GmkVector2 b);
float gmk_vector2_cross (GmkVector2 a, GmkVector2 b);
float gmk_vector2_angle (GmkVector2 v);
GmkVector2 gmk_scm_to_vector2 (SCM s_v);
SCM gmk_scm_from_vector2 (GmkVector2 v);
GmkVector2 gmk_vector2_zero ();
GmkVector2 gmk_vector2_new (float x, float y);
GmkVector2 gmk_vector2_add (GmkVector2 a, GmkVector2 b);
GmkVector2 gmk_vector2_sub (GmkVector2 a, GmkVector2 b);
GmkVector2 gmk_vector2_scale (GmkVector2 v, float scalar);
GmkVector2 gmk_vector2_norm (GmkVector2 v);
GmkVector2 gmk_vector2_from_polar (float radius, float angle);
GmkVector2 gmk_vector2_right_normal (GmkVector2 v);
GmkVector2 gmk_vector2_left_normal (GmkVector2 v);

/* Scheme bindings. */
SCM gmk_make_vector2 (SCM x, SCM y);
SCM gmk_s_vector2_from_polar (SCM radius, SCM angle);
SCM gmk_s_vector2_x (SCM v);
SCM gmk_s_vector2_y (SCM v);
SCM gmk_s_vector2_add (SCM vectors);
SCM gmk_s_vector2_sub (SCM v, SCM vectors);
SCM gmk_s_vector2_scale (SCM v, SCM scalar);
SCM gmk_s_vector2_norm (SCM v);
SCM gmk_s_vector2_mag (SCM v);
SCM gmk_s_vector2_angle (SCM v);
SCM gmk_s_vector2_dot (SCM v1, SCM v2);
SCM gmk_s_vector2_cross (SCM v1, SCM v2);
SCM gmk_s_vector2_right_normal (SCM v);
SCM gmk_s_vector2_left_normal (SCM v);
void gmk_init_vector2 (void);

#endif
