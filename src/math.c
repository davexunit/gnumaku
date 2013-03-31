#include "math.h"
#include <math.h>
#include <stdlib.h>
#include <allegro5/allegro.h>

float gmk_deg_to_rad (float degrees)
{
    return degrees * ALLEGRO_PI / 180;
}

float gmk_rad_to_deg (float radians)
{
    return radians * 180 / ALLEGRO_PI;
}

float
gmk_rand1 (void)
{
    return (float) rand () / (float) RAND_MAX;
}

float gmk_randf (float max)
{
    return gmk_rand1 () * max;
}

float
gmk_clamp (float n, float min, float max)
{
    return fmax (min, fmin (max, n));
}
