#ifndef BULLET_SYSTEM_H
#define BULLET_SYSTEM_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_primitives.h>
#include <libguile.h>

#include "math.h"
#include "sprite_sheet.h"
#include "rect.h"

typedef struct {
    float speed;
    float direction;
    float acceleration;
    float angular_velocity;
    float x, y;
    bool active;
    bool referenced;
    bool killable;
    Rect hitbox;
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
} Bullet;

typedef struct {
    int max_bullets;
    int bullet_count;
    Bullet *bullets;
    SCM sprite_sheet;
    Rect bounds;
} BulletSystem;

typedef struct {
    BulletSystem *bullet_system;
    Bullet *bullet;
} BulletRef;

void init_bullet_system_type (void);

#endif
