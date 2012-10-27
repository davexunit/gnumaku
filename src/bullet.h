#ifndef BULLET_H
#define BULLET_H

#include <allegro5/allegro.h>

#include "math.h"

typedef struct {
    float speed;
    float direction;
    float acceleration;
    float angular_velocity;
    float x, y;
    float dx, dy;
    float ddx, ddy;
} Bullet;

#endif
