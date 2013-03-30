#include "vector.h"
#include "math.h"

bool
gmk_vector2_equal (GmkVector2 a, GmkVector2 b)
{
    return a.x == b.x && a.y == b.y;
}

float
gmk_vector2_mag (GmkVector2 v)
{
    return sqrt (v.x * v.x + v.y * v.y);
}

float
gmk_vector2_dot (GmkVector2 a, GmkVector2 b)
{
    return a.x * b.x + a.y * b.y;
}

float
gmk_vector2_cross (GmkVector2 a, GmkVector2 b)
{
    return a.x * b.y - b.x * a.y;
}

float
gmk_vector2_angle (GmkVector2 v)
{
    return atan2 (v.y, v.x);
}

GmkVector2
gmk_vector2_zero ()
{
    GmkVector2 v;

    v.x = 0;
    v.y = 0;

    return v;
}

GmkVector2
gmk_vector2_new (float x, float y)
{
    GmkVector2 v;

    v.x = x;
    v.y = y;

    return v;
}

GmkVector2
gmk_vector2_add (GmkVector2 a, GmkVector2 b)
{
    GmkVector2 c;

    c.x = a.x + b.x;
    c.y = a.y + b.y;

    return c;
}

GmkVector2
gmk_vector2_sub (GmkVector2 a, GmkVector2 b)
{
    GmkVector2 c;

    c.x = a.x - b.x;
    c.y = a.y - b.y;

    return c;
}

GmkVector2
gmk_vector2_scale (GmkVector2 v, float scalar)
{
    GmkVector2 a;

    a.x = v.x * scalar;
    a.y = v.y * scalar;

    return a;
}

GmkVector2
gmk_vector2_norm (GmkVector2 v)
{
    float m = gmk_vector2_mag (v);
    GmkVector2 n = gmk_vector2_zero ();

    if (m != 0) {
        n.x = v.x / m;
        n.y = v.y / m;
    }

    return n;
}

GmkVector2
gmk_vector2_from_polar (float radius, float angle)
{
    GmkVector2 v;

    v.x = cos (angle) * radius;
    v.y = sin (angle) * radius;

    return v;
}

GmkVector2
gmk_vector2_right_normal (GmkVector2 v)
{
    GmkVector2 right;

    right.x = -v.y;
    right.y = v.x;

    return right;
}

GmkVector2
gmk_vector2_left_normal (GmkVector2 v)
{
    GmkVector2 left;

    left.x = v.y;
    left.y = -v.x;

    return left;
}
