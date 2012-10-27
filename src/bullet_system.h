#ifndef BULLET_SYSTEM_H
#define BULLET_SYSTEM_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>

#include "math.h"

typedef struct {
    float speed;
    float direction;
    float acceleration;
    float angular_velocity;
    float x, y;
    float dx, dy;
    float ddx, ddy;
    bool alive;
    ALLEGRO_BITMAP *image;
} Bullet;

typedef struct {
    int max_bullets;
    Bullet *bullets;
    ALLEGRO_BITMAP *image;
} BulletSystem;

void init_bullet_system_type(void);

#endif
