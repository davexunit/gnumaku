#ifndef GMK_PARTICLE_SYSTEM_H
#define GMK_PARTICLE_SYSTEM_H

#include "common.h"
#include "color.h"
#include "sprite_sheet.h"

typedef struct {
    ALLEGRO_BITMAP *image;
    ALLEGRO_COLOR color;
    ALLEGRO_COLOR dcolor;
    GmkVector2 pos;
    GmkVector2 vel;
    float radial_accel;
    float tan_accel;
    float scale;
    float dscale;
    int life;
    int duration;
    bool active;
} Particle;

typedef struct {
    SCM sprite_sheet;
    Particle *particles;
    bool blend_additive;
    int max_particles;
    int particle_count;
    int timer;
    int rate;
    int amount;
    int life;
    int life_var;
    GmkVector2 pos;
    GmkVector2 pos_var;
    GmkVector2 gravity;
    GmkVector2 gravity_var;
    float direction;
    float direction_var;
    float speed;
    float speed_var;
    float radial_accel;
    float radial_accel_var;
    float tan_accel;
    float tan_accel_var;
    float start_scale;
    float start_scale_var;
    float end_scale;
    float end_scale_var;
    ALLEGRO_COLOR start_color;
    ALLEGRO_COLOR start_color_var;
    ALLEGRO_COLOR end_color;
    ALLEGRO_COLOR end_color_var;
} ParticleSystem;

SCM gmk_s_make_particle_system (SCM max_particles, SCM sprite_sheet);
SCM gmk_s_update_particle_system (SCM particle_system);
SCM gmk_s_draw_particle_system (SCM particle_system);
SCM gmk_s_particle_sprite_sheet (SCM particle_system);
SCM gmk_s_particle_blend_additive (SCM particle_system);
SCM gmk_s_max_particles (SCM particle_system);
SCM gmk_s_particle_count (SCM particle_system);
SCM gmk_s_particle_rate (SCM particle_system);
SCM gmk_s_particle_amount (SCM particle_system);
SCM gmk_s_particle_life (SCM particle_system);
SCM gmk_s_particle_life_var (SCM particle_system);
SCM gmk_s_particle_position (SCM particle_system);
SCM gmk_s_particle_position_var (SCM particle_system);
SCM gmk_s_particle_gravity (SCM particle_system);
SCM gmk_s_particle_direction (SCM particle_system);
SCM gmk_s_particle_direction_var (SCM particle_system);
SCM gmk_s_particle_speed (SCM particle_system);
SCM gmk_s_particle_speed_var (SCM particle_system);
SCM gmk_s_particle_radial_accel (SCM particle_system);
SCM gmk_s_particle_radial_accel_var (SCM particle_system);
SCM gmk_s_particle_tan_accel (SCM particle_system);
SCM gmk_s_particle_tan_accel_var (SCM particle_system);
SCM gmk_s_particle_start_scale (SCM particle_system);
SCM gmk_s_particle_start_scale_var (SCM particle_system);
SCM gmk_s_particle_end_scale (SCM particle_system);
SCM gmk_s_particle_end_scale_var (SCM particle_system);
SCM gmk_s_particle_start_color (SCM particle_system);
SCM gmk_s_particle_start_color_var (SCM particle_system);
SCM gmk_s_particle_end_color (SCM particle_system);
SCM gmk_s_particle_end_color_var (SCM particle_system);
SCM gmk_s_set_particle_sprite_sheet (SCM particle_system, SCM sprite_sheet);
SCM gmk_s_set_particle_blend_additive (SCM particle_system, SCM blend_additive);
SCM gmk_s_set_particle_rate (SCM particle_system, SCM rate);
SCM gmk_s_set_particle_amount (SCM particle_system, SCM amount);
SCM gmk_s_set_particle_life (SCM particle_system, SCM life);
SCM gmk_s_set_particle_life_var (SCM particle_system, SCM life_var);
SCM gmk_s_set_particle_position (SCM particle_system, SCM position);
SCM gmk_s_set_particle_position_var (SCM particle_system, SCM position_var);
SCM gmk_s_set_particle_gravity (SCM particle_system, SCM gravity);
SCM gmk_s_set_particle_direction (SCM particle_system, SCM direction);
SCM gmk_s_set_particle_direction_var (SCM particle_system, SCM direction_var);
SCM gmk_s_set_particle_speed (SCM particle_system, SCM speed);
SCM gmk_s_set_particle_speed_var (SCM particle_system, SCM speed_var);
SCM gmk_s_set_particle_radial_accel (SCM particle_system, SCM radial_accel);
SCM gmk_s_set_particle_radial_accel_var (SCM particle_system, SCM radial_accel_var);
SCM gmk_s_set_particle_tan_accel (SCM particle_system, SCM tan_accel);
SCM gmk_s_set_particle_tan_accel_var (SCM particle_system, SCM tan_accel_var);
SCM gmk_s_set_particle_start_scale (SCM particle_system, SCM start_scale);
SCM gmk_s_set_particle_start_scale_var (SCM particle_system, SCM start_scale_var);
SCM gmk_s_set_particle_end_scale (SCM particle_system, SCM end_scale);
SCM gmk_s_set_particle_end_scale_var (SCM particle_system, SCM end_scale_var);
SCM gmk_s_set_particle_start_color (SCM particle_system, SCM start_color);
SCM gmk_s_set_particle_start_color_var (SCM particle_system, SCM start_color_var);
SCM gmk_s_set_particle_end_color (SCM particle_system, SCM end_color);
SCM gmk_s_set_particle_end_color_var (SCM particle_system, SCM end_color_var);
void gmk_init_particle_system (void);

#endif
