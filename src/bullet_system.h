#ifndef BULLET_SYSTEM_H
#define BULLET_SYSTEM_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_primitives.h>
#include <libguile.h>

#include "math.h"
#include "vector.h"
#include "sprite_sheet.h"
#include "rect.h"
#include "color.h"
#include "blend_mode.h"
#include "bullet_type.h"

/* God damn this struct has gotten quite large. */
typedef struct {
    int id; /* Unique identifier. */
    int life; /* Maximum lifetime. 0 is unlimited. */
    int script_time; /* Time to execute the bullet script. */
    int life_count; /* Total elapsed lifetime. */
    bool active; /* Currently being drawn/updated? */
    bool kill; /* Remove the bullet on next update? */
    bool directional; /* Rotate sprite in bullet direction? */
    Vector2 pos;
    Vector2 vel;
    Vector2 acc;
    Vector2 scale;
    Rect hitbox;
    GmkBlendMode blend_mode;
    ALLEGRO_TRANSFORM angular_velocity; /* Change in direction. */
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
    SCM script; /* Scheme procedure to run at a given time. */
    SCM ref; /* Scheme structure for referencing a bullet. */
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
