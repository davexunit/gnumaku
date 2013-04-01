#ifndef GMK_PARTICLE_SYSTEM_H
#define GMK_PARTICLE_SYSTEM_H

#include <allegro5/allegro.h>
#include "vector.h"
#include "rect.h"

/*
 * This module provides a generic particle system with a Guile API.
 * GmkParticleSystem serves as a base for different types of systems.
 * A more traditional particle system and Gnumaku's bullet system can
 * be built upon this.
 */

/*
 * The physical representation of a particle.
 *
 * Member variables beginning with an underscore denote cached values
 * for more efficient updating.
 */
typedef struct {
    float speed;
    float accel;
    float direction;
    float ang_vel;
    GmkVector2 pos;
    GmkVector2 scale;
    GmkVector2 _vel;
    GmkVector2 _accel;
    ALLEGRO_TRANSFORM _ang_vel;
    GmkRect hitbox;
} GmkParticleBody;

typedef struct {
    int op;
    int src;
    int dst;
} GmkBlender;

typedef struct {
    int id;
    /* Maximum lifetime. 0 is unlimited. */
    int max_lifetime;
    int lifetime;
    /* Particles are removed when this flag is set. */
    bool kill;
    /* Rotate sprite in particle direction? */
    bool directional;
    ALLEGRO_BITMAP *bitmap;
    ALLEGRO_COLOR color;
    /* Guile user data object. */
    SCM data;
    GmkParticleBody body;
    GmkBlender blend;
} GmkParticle;

typedef struct {
    int max_particles;
    int particle_count;
    GmkParticle *particles;
    /* Maps bullet id to bullet pool index for quick lookups. */
    int *particle_ids;
} GmkParticleSystem;

void gmk_inixot_particle_system (void);

#endif
