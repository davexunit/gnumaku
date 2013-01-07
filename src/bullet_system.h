#ifndef BULLET_SYSTEM_H
#define BULLET_SYSTEM_H

#include "common.h"
#include "sprite_sheet.h"
#include "rect.h"
#include "color.h"
#include "blend_mode.h"
#include "bullet_type.h"

/* God damn this struct has gotten quite large. */
typedef struct {
    int id;           /* Unique identifier. */
    int life;         /* Maximum lifetime. 0 is unlimited. */
    int script_time;  /* Time to execute the bullet script. */
    int life_count;   /* Total elapsed lifetime. */
    bool active;      /* Currently being drawn/updated? */
    bool kill;        /* Remove the bullet on next update? */
    bool directional; /* Rotate sprite in bullet direction? */
    GmkVector2 pos;
    GmkVector2 vel;
    GmkVector2 acc;
    GmkVector2 scale;
    GmkRect hitbox;
    GmkBlendMode blend_mode;
    ALLEGRO_TRANSFORM angular_velocity; /* Change in direction. */
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
    SCM script; /* Scheme procedure to run at a given time. */
    SCM ref;    /* Scheme structure for referencing a bullet. */
} GmkBullet;

typedef struct {
    int max_bullets;
    int bullet_count;
    GmkBullet *bullets;
    int *bullet_ids; /* Maps bullet id to bullet pool index. */
    SCM sprite_sheet;
    GmkRect bounds;
} GmkBulletSystem;

typedef struct {
    GmkBulletSystem *system;
    int id;
} GmkBulletRef;

SCM gmk_s_make_bullet_system (SCM max_bullets, SCM sprite_sheet);
SCM gmk_s_clear_bullet_system (SCM bullet_system);
SCM gmk_s_max_bullets (SCM bullet_system);
SCM gmk_s_draw_bullet_system (SCM bullet_system);
SCM gmk_s_draw_bullet_system_hitboxes (SCM bullet_system);
SCM gmk_s_update_bullet_system (SCM bullet_system);
SCM gmk_s_set_bullet_system_sprite_sheet (SCM bullet_system, SCM sprite_sheet);
SCM gmk_s_bullet_system_sprite_sheet (SCM bullet_system);
SCM gmk_s_bullet_system_collide_rect (SCM bullet_system, SCM rect, SCM callback);
SCM gmk_s_bullet_system_bounds (SCM bullet_system);
SCM gmk_s_bullet_system_count (SCM bullet_system);
SCM gmk_s_set_bullet_system_bounds (SCM bullet_system, SCM rect);
SCM gmk_s_emit_bullet (SCM bullet_system, SCM pos, SCM speed, SCM direction,
                       SCM type, SCM kw_args);
SCM gmk_s_emit_script_bullet (SCM bullet_system, SCM pos, SCM type, SCM script);
SCM gmk_s_set_bullet_movement (SCM bullet_ref, SCM speed, SCM direction,
                               SCM acceleration, SCM angular_velocity);
SCM gmk_s_kill_bullet (SCM bullet_ref);
SCM gmk_s_bullet_position (SCM bullet_ref);
SCM gmk_s_bullet_speed (SCM bullet_ref);
SCM gmk_s_bullet_direction (SCM bullet_ref);
SCM gmk_s_bullet_acceleration (SCM bullet_ref);
SCM gmk_s_bullet_angular_velocity (SCM bullet_ref);
SCM gmk_s_bullet_life (SCM bullet_ref);
SCM gmk_s_bullet_color (SCM bullet_ref);
SCM gmk_s_bullet_scale (SCM bullet_ref);
SCM gmk_s_set_bullet_script (SCM bullet_ref, SCM script, SCM dt);
SCM gmk_s_set_bullet_type (SCM bullet_ref, SCM bull_type);
SCM gmk_s_set_bullet_position  (SCM bullet_ref, SCM pos);
SCM gmk_s_set_bullet_speed (SCM bullet_ref, SCM speed);
SCM gmk_s_set_bullet_direction (SCM bullet_ref, SCM direction);
SCM gmk_s_set_bullet_acceleration (SCM bullet_ref, SCM acceleration);
SCM gmk_s_set_bullet_angular_velocity (SCM bullet_ref, SCM angular_velocity);
SCM gmk_s_set_bullet_life (SCM bullet_ref, SCM life);
SCM gmk_s_set_bullet_color (SCM bullet_ref, SCM color);
SCM gmk_s_set_bullet_scale (SCM bullet_ref, SCM scale);
void gmk_init_bullet_system (void);

#endif
