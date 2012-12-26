#ifndef BULLET_SYSTEM_H
#define BULLET_SYSTEM_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_primitives.h>
#include <libguile.h>

#include "math.h"
#include "sprite_sheet.h"
#include "rect.h"

/* String representations of Scheme symbols for blend modes */
#define SYM_BLEND_ALPHA "alpha"
#define SYM_BLEND_ADD   "add"

typedef enum {
    BLEND_ALPHA,
    BLEND_ADD,
} BlendMode;

typedef struct {
    int id; /* Unique identifier. */
    int life; /* Maximum lifetime. 0 is unlimited. */
    int life_count; /* Total elapsed lifetime */
    bool active; /* Currently being drawn/updated? */
    bool scripted; /* Scripted bullets must be freed explicitly by the user. */
    float x, y;     /* Position */
    float dx, dy;   /* Velocity */
    float ddx, ddy; /* Acceleration */
    ALLEGRO_TRANSFORM angular_velocity; /* Change in direction */
    Rect hitbox;
    BlendMode blend_mode;
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
} Bullet;

typedef struct {
    int max_bullets;
    int bullet_count;
    Bullet *bullets;
    int *bullet_ids; /* Maps bullet id to bullet pool index. */
    SCM sprite_sheet;
    Rect bounds;
} BulletSystem;

typedef struct {
    BulletSystem *system;
    int id;
} BulletRef;

void init_bullet_system_type (void);

#endif
