#ifndef GNUMAKU_PARTICLE_SYSTEM_H
#define GNUMAKU_PARTICLE_SYSTEM_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <libguile.h>

#include "math.h"
#include "vector.h"
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

void init_particle_system_type (void);

#endif
