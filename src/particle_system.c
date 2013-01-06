#include "particle_system.h"

static scm_t_bits particle_system_tag;

static ALLEGRO_COLOR
variable_color (ALLEGRO_COLOR color, ALLEGRO_COLOR color_var)
{
    float r, b, g, a;
    float vr, vb, vg, va;

    al_unmap_rgba_f (color, &r, &g, &b, &a);
    al_unmap_rgba_f (color_var, &vr, &vg, &vb, &va);

    return al_map_rgba_f (r + vr * gmk_rand1 (),
                          g + vg * gmk_rand1 (),
                          b + vb * gmk_rand1 (),
                          a + va * gmk_rand1 ());
}

static ALLEGRO_COLOR
delta_color (ALLEGRO_COLOR start_color, ALLEGRO_COLOR end_color, int duration)
{
    float r1, b1, g1, a1;
    float r2, b2, g2, a2;

    al_unmap_rgba_f (start_color, &r1, &g1, &b1, &a1);
    al_unmap_rgba_f (end_color, &r2, &g2, &b2, &a2);

    return al_map_rgba_f ((r2 - r1) / duration,
                          (g2 - g1) / duration,
                          (b2 - b1) / duration,
                          (a2 - a1) / duration);
}

static ParticleSystem *
check_particle_system (SCM particle_system)
{
    scm_assert_smob_type (particle_system_tag, particle_system);

    return (ParticleSystem *) SCM_SMOB_DATA (particle_system);
}

static void
init_particle (ParticleSystem *system, Particle *particle)
{
    ALLEGRO_COLOR start_color = variable_color (system->start_color,
                                                system->start_color_var);
    ALLEGRO_COLOR end_color = variable_color (system->end_color,
                                              system->end_color_var);
    GmkSpriteSheet *sprite_sheet = gmk_scm_to_sprite_sheet (system->sprite_sheet);
    int tile = (int) sprite_sheet->num_tiles * gmk_rand1 ();
    float theta = gmk_deg_to_rad (system->direction +
                                  gmk_randf (system->direction_var));
    float speed = system->speed + gmk_randf (system->speed_var);
    float start_scale = system->start_scale + gmk_randf (system->start_scale_var);
    float end_scale = system->end_scale + gmk_randf (system->end_scale_var);

    /* Messy. */
    particle->life = (int) system->life + gmk_randf (system->life_var);
    particle->image = gmk_sprite_sheet_tile (sprite_sheet, tile);
    particle->color = start_color;
    particle->dcolor = delta_color (start_color, end_color, particle->life);
    particle->pos.x = system->pos.x + gmk_randf (system->pos_var.x);
    particle->pos.y = system->pos.y + gmk_randf (system->pos_var.y);
    particle->vel = gmk_vector2_from_polar (speed, theta);
    particle->radial_accel = system->radial_accel + gmk_randf (system->radial_accel_var);
    particle->tan_accel = system->tan_accel + gmk_randf (system->tan_accel_var);
    particle->scale = start_scale;
    particle->dscale = (end_scale - start_scale) / particle->life;
    particle->duration = 0;
    particle->active = true;
}

SCM_DEFINE (gmk_s_make_particle_system, "make-particle-system", 2, 0, 0,
            (SCM max_particles, SCM sprite_sheet),
            "Make a new particle system instance.")
{
    SCM smob;
    ParticleSystem *system;

    system = (ParticleSystem *) scm_gc_malloc (sizeof (ParticleSystem),
                                               "particle system");
    system->particles = NULL;
    system->max_particles = scm_to_int (max_particles);
    system->particle_count = 0;
    system->blend_additive = true;
    system->timer = 0;
    system->rate = 0;
    system->amount = 0;
    system->life = 0;
    system->life_var = 0;
    system->pos.x = 0;
    system->pos.y = 0;
    system->pos_var.y = 0;
    system->pos_var.x = 0;
    system->radial_accel = 0;
    system->radial_accel_var = 0;
    system->tan_accel = 0;
    system->tan_accel_var = 0;
    system->direction = 0;
    system->direction_var = 0;
    system->speed = 0;
    system->speed_var = 0;
    system->start_scale = 1;
    system->start_scale_var = 0;
    system->end_scale = 1;
    system->end_scale_var = 0;
    system->start_color = al_map_rgba_f (1, 1, 1, 1);
    system->start_color_var = al_map_rgba_f (0, 0, 0, 0);
    system->end_color = al_map_rgba_f (0, 0, 0, 0);
    system->end_color_var = al_map_rgba_f (0, 0, 0, 0);

    SCM_NEWSMOB (smob, particle_system_tag, system);

    system->particles =
        (Particle *) scm_gc_malloc (sizeof (Particle) * system->max_particles,
                                    "particles");
    system->sprite_sheet = sprite_sheet;

    return smob;
}

static void
draw_particle (Particle *particle)
{
    if (particle->active) {
        float cx = al_get_bitmap_width (particle->image) / 2;
        float cy = al_get_bitmap_height (particle->image) / 2;
        float x = particle->pos.x - (cx * particle->scale);
        float y = particle->pos.y - (cy * particle->scale);

        al_draw_tinted_scaled_rotated_bitmap(particle->image, particle->color,
                                             0, 0, x, y,
                                             particle->scale, particle->scale,
                                             0, 0);
    }
}

SCM_DEFINE (gmk_s_draw_particle_system, "draw-particle-system", 1, 0, 0,
            (SCM particle_system),
            "Draw particle system.")
{
    ParticleSystem *system = check_particle_system (particle_system);
    ALLEGRO_STATE state;

    al_store_state (&state, ALLEGRO_STATE_BLENDER);

    if (system->blend_additive) {
        al_set_blender(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
    }

    al_hold_bitmap_drawing (true);

    for (int i = 0; i < system->particle_count; ++i) {
        draw_particle (&system->particles[i]);
    }

    al_hold_bitmap_drawing (false);
    al_restore_state (&state);

    return SCM_UNSPECIFIED;
}

static Particle *
get_free_particle (ParticleSystem *system)
{
    if (system->particle_count < system->max_particles) {
        return &system->particles[system->particle_count++];
    }

    return NULL;
}

static void
free_particle (ParticleSystem *system, int index)
{
    Particle temp;
    Particle *particle =  &system->particles[index];
    int particle_count = --system->particle_count;

    /* Swap dead particle with active particle.
     * This keeps the array of particles balanced.
     * All of the active particles will be grouped together,
     * sequentially, from the beginning of the array.
     */
    particle->active = false;
    temp = system->particles[particle_count];
    system->particles[particle_count] = *particle;
    system->particles[index] = temp;
}

static void
particle_system_do_emit (ParticleSystem *system)
{
    for (int i = 0; i < system->amount; ++i) {
        Particle *particle = get_free_particle (system);

        if (particle) {
            init_particle (system, particle);
        }
    }
}


static void
update_particle (ParticleSystem *system, int index)
{
    Particle *particle = &system->particles[index];
    GmkVector2 radial = gmk_vector2_zero();
    GmkVector2 tangent;

    if (particle->active) {
        if (!gmk_vector2_equal (particle->pos, system->pos)) {
            radial = gmk_vector2_norm (gmk_vector2_sub (system->pos, particle->pos));
        }

        tangent = gmk_vector2_right_normal (radial);
        radial = gmk_vector2_scale (radial, particle->radial_accel);
        tangent = gmk_vector2_scale (tangent, particle->tan_accel);
        particle->vel = gmk_vector2_add (particle->vel,
                                         (gmk_vector2_add (radial, tangent)));
        particle->pos = gmk_vector2_add (particle->pos, particle->vel);
        particle->scale += particle->dscale;
        particle->color = gmk_add_color (particle->color, particle->dcolor);
        ++particle->duration;

        if (particle->duration >= particle->life) {
            free_particle (system, index);
        }
    }
}

SCM_DEFINE (gmk_s_update_particle_system, "update-particle-system", 1, 0, 0,
            (SCM particle_system),
            "Update particle system.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    for (int i = 0; i < system->particle_count; ++i) {
        update_particle (system, i);
    }

    if (system->rate != 0) {
        ++system->timer;

        if (system->timer >= system->rate) {
            system->timer = 0;
            particle_system_do_emit (system);
        }
    }

    scm_remember_upto_here_1 (particle_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_particle_sprite_sheet, "particle-sprite-sheet", 1, 0, 0,
            (SCM particle_system),
            "Return sprite sheet for particle system.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return system->sprite_sheet;
}

SCM_DEFINE (gmk_s_particle_blend_additive, "particle-blend-additive?", 1, 0, 0,
            (SCM particle_system),
            "Return @code{#t} if additive blending is used.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_bool (system->blend_additive);
}

SCM_DEFINE (gmk_s_max_particles, "max-particles", 1, 0, 0,
            (SCM particle_system),
            "Return maximum number of particles for @var{particle_system}")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->max_particles);
}

SCM_DEFINE (gmk_s_particle_count, "particle-count", 1, 0, 0,
            (SCM particle_system),
            "Return number of active particles.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->particle_count);
}

SCM_DEFINE (gmk_s_particle_rate, "particle-rate", 1, 0, 0,
            (SCM particle_system),
            "Return rate at which particles are emitted.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->rate);
}

SCM_DEFINE (gmk_s_particle_amount, "particle-amount", 1, 0, 0,
            (SCM particle_system),
            "Return the number of particles that emitted at a time.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->amount);
}

SCM_DEFINE (gmk_s_particle_life, "particle-life", 1, 0, 0,
            (SCM particle_system),
            "Return the minimum lifetime of each particle.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->life);
}

SCM_DEFINE (gmk_s_particle_life_var, "particle-life-var", 1, 0, 0,
            (SCM particle_system),
            "Return the lifetime variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->life_var);
}

SCM_DEFINE (gmk_s_particle_position, "particle-position", 1, 0, 0,
            (SCM particle_system),
            "Return the position from which particles are emitted from.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_vector2 (system->pos);
}

SCM_DEFINE (gmk_s_particle_position_var, "particle-position-var", 1, 0, 0,
            (SCM particle_system),
            "Return the position variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_vector2 (system->pos_var);
}

SCM_DEFINE (gmk_s_particle_gravity, "particle-gravity", 1, 0, 0,
            (SCM particle_system),
            "Return the particle system gravity.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_vector2 (system->gravity);
}

SCM_DEFINE (gmk_s_particle_direction, "particle-direction", 1, 0, 0,
            (SCM particle_system),
            "Return the direction in which particles are emitted.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->direction);
}

SCM_DEFINE (gmk_s_particle_direction_var, "particle-direction-var", 1, 0, 0,
            (SCM particle_system),
            "Return the direction variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->direction_var);
}

SCM_DEFINE (gmk_s_particle_speed, "particle-speed", 1, 0, 0,
            (SCM particle_system),
            "Return the speed with which particles are emitted.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->speed);
}

SCM_DEFINE (gmk_s_particle_speed_var, "particle-speed-var", 1, 0, 0,
            (SCM particle_system),
            "Return the speed variance")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->speed_var);
}

SCM_DEFINE (gmk_s_particle_radial_accel, "particle-radial-accel", 1, 0, 0,
            (SCM particle_system),
            "Return the radial acceleration of particles.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->radial_accel);
}

SCM_DEFINE (gmk_s_particle_radial_accel_var, "particle-radial-accel-var", 1, 0, 0,
            (SCM particle_system),
            "Return the radial acceleration variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->radial_accel_var);
}

SCM_DEFINE (gmk_s_particle_tan_accel, "particle-tan-accel", 1, 0, 0,
            (SCM particle_system),
            "Return the tangential acceleration of particles.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->tan_accel);
}

SCM_DEFINE (gmk_s_particle_tan_accel_var, "particle-tan-accel-var", 1, 0, 0,
            (SCM particle_system),
            "Return the tangential acceleration variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->tan_accel_var);
}

SCM_DEFINE (gmk_s_particle_start_scale, "particle-start-scale", 1, 0, 0,
            (SCM particle_system),
            "Return the initial particle scale vector.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->start_scale);
}

SCM_DEFINE (gmk_s_particle_start_scale_var, "particle-start-scale-var", 1, 0, 0,
            (SCM particle_system),
            "Return the initial scale variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->start_scale_var);
}

SCM_DEFINE (gmk_s_particle_end_scale, "particle-end-scale", 1, 0, 0,
            (SCM particle_system),
            "Return the final particle scale vector.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->end_scale);
}

SCM_DEFINE (gmk_s_particle_end_scale_var, "particle-end-scale-var", 1, 0, 0,
            (SCM particle_system),
            "Return the final scale variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return scm_from_double (system->end_scale_var);
}

SCM_DEFINE (gmk_s_particle_start_color, "particle-start-color", 1, 0, 0,
            (SCM particle_system),
            "Return the initial color of particles.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_color (system->start_color);
}

SCM_DEFINE (gmk_s_particle_start_color_var, "particle-start-color-var", 1, 0, 0,
            (SCM particle_system),
            "Return the initial color variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_color (system->start_color_var);
}

SCM_DEFINE (gmk_s_particle_end_color, "particle-end-color", 1, 0, 0,
            (SCM particle_system),
            "Return the final color of particles.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_color (system->end_color);
}

SCM_DEFINE (gmk_s_particle_end_color_var, "particle-end-color-var", 1, 0, 0,
            (SCM particle_system),
            "Return the final color variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    return gmk_scm_from_color (system->end_color_var);
}

SCM_DEFINE (gmk_s_set_particle_sprite_sheet, "set-particle-sprite-sheet", 2, 0, 0,
            (SCM particle_system, SCM sprite_sheet),
            "Set the particle system sprite sheet.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->sprite_sheet = sprite_sheet;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_blend_additive, "set-particle-blend-additive", 2, 0, 0,
            (SCM particle_system, SCM blend_additive),
            "Set the particle system blend mode.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->blend_additive = scm_to_bool (blend_additive);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_rate, "set-particle-rate", 2, 0, 0,
            (SCM particle_system, SCM rate),
            "Set the particle emission rate.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->rate = scm_to_int (rate);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_amount, "set-particle-amount", 2, 0, 0,
            (SCM particle_system, SCM amount),
            "Set the particle emission amount.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->amount = scm_to_int (amount);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_life, "set-particle-life", 2, 0, 0,
            (SCM particle_system, SCM life),
            "Set the particle lifetime.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->life = scm_to_int (life);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_life_var, "set-particle-life-var", 2, 0, 0,
            (SCM particle_system, SCM life_var),
            "Set the particle lifetime variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->life_var = scm_to_int (life_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_position, "set-particle-position", 2, 0, 0,
            (SCM particle_system, SCM position),
            "Set the particle position.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->pos = gmk_scm_to_vector2 (position);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_position_var, "set-particle-position-var", 2, 0, 0,
            (SCM particle_system, SCM position_var),
            "Set the particle position variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->pos_var = gmk_scm_to_vector2 (position_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_gravity, "set-particle-gravity", 2, 0, 0,
            (SCM particle_system, SCM gravity),
            "Set the particle gravity.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->gravity = gmk_scm_to_vector2 (gravity);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_direction, "set-particle-direction", 2, 0, 0,
            (SCM particle_system, SCM direction),
            "Set the particle direction.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->direction = scm_to_double (direction);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_direction_var, "set-particle-direction-var", 2, 0, 0,
            (SCM particle_system, SCM direction_var),
            "Set the particle direction variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->direction_var = scm_to_double (direction_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_speed, "set-particle-speed", 2, 0, 0,
            (SCM particle_system, SCM speed),
            "Set the particle speed.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->speed = scm_to_double (speed);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_speed_var, "set-particle-speed-var", 2, 0, 0,
            (SCM particle_system, SCM speed_var),
            "Set the particle speed variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->speed_var = scm_to_double (speed_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_radial_accel, "set-particle-radial-accel", 2, 0, 0,
            (SCM particle_system, SCM radial_accel),
            "Set the particle radial acceleration.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->radial_accel = scm_to_double (radial_accel);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_radial_accel_var,
            "set-particle-radial-accel-var", 2, 0, 0,
            (SCM particle_system, SCM radial_accel_var),
            "Set the particle radial acceleration variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->radial_accel_var = scm_to_double (radial_accel_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_tan_accel, "set-particle-tan-accel", 2, 0, 0,
            (SCM particle_system, SCM tan_accel),
            "Set the particle tangential acceleration.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->tan_accel = scm_to_double (tan_accel);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_tan_accel_var, "set-particle-tan-accel-var", 2, 0, 0,
            (SCM particle_system, SCM tan_accel_var),
            "Set the particle tangential acceleration variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->tan_accel_var = scm_to_double (tan_accel_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_start_scale, "set-particle-start-scale", 2, 0, 0,
            (SCM particle_system, SCM start_scale),
            "Set the particle initial scale.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->start_scale = scm_to_double (start_scale);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_start_scale_var,
            "set-particle-start-scale-var", 2, 0, 0,
            (SCM particle_system, SCM start_scale_var),
            "Set the particle initial scale variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->start_scale_var = scm_to_double (start_scale_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_end_scale, "set-particle-end-scale", 2, 0, 0,
            (SCM particle_system, SCM end_scale),
            "Set the particle final scale.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->end_scale = scm_to_double (end_scale);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_end_scale_var,
            "set-particle-end-scale-var", 2, 0, 0,
            (SCM particle_system, SCM end_scale_var),
            "Set the particle final scale variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->end_scale_var = scm_to_double (end_scale_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_start_color, "set-particle-start-color", 2, 0, 0,
            (SCM particle_system, SCM start_color),
            "Set the particle initial color.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->start_color = gmk_scm_to_color (start_color);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_start_color_var,
            "set-particle-start-color-var", 2, 0, 0,
            (SCM particle_system, SCM start_color_var),
            "Set the particle initial color variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->start_color_var = gmk_scm_to_color (start_color_var);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_end_color, "set-particle-end-color", 2, 0, 0,
            (SCM particle_system, SCM end_color),
            "Set the particle final color.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->end_color = gmk_scm_to_color (end_color);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_particle_end_color_var,
            "set-particle-end-color-var", 2, 0, 0,
            (SCM particle_system, SCM end_color_var),
            "Set the particle final color variance.")
{
    ParticleSystem *system = check_particle_system (particle_system);

    system->end_color_var = gmk_scm_to_color (end_color_var);

    return SCM_UNSPECIFIED;
}

static SCM
mark_particle_system (SCM particle_system)
{
    ParticleSystem *system = (ParticleSystem *) SCM_SMOB_DATA (particle_system);

    return system->sprite_sheet;
}

static size_t
free_particle_system (SCM particle_system)
{
    ParticleSystem *system = (ParticleSystem *) SCM_SMOB_DATA (particle_system);

    scm_gc_free (system->particles,
                 sizeof (Particle) * system->max_particles, "particles");
    scm_gc_free (system, sizeof (ParticleSystem), "particle system");

    return 0;
}

static int
print_particle_system (SCM particle_system, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<particle-system max-particles: ", port);
    scm_display (gmk_s_max_particles (particle_system), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_particle_system (void) {
    particle_system_tag = scm_make_smob_type ("<particle-system>",
                                              sizeof (ParticleSystem));
    scm_set_smob_mark (particle_system_tag, mark_particle_system);
    scm_set_smob_free (particle_system_tag, free_particle_system);
    scm_set_smob_print (particle_system_tag, print_particle_system);

#include "particle_system.x"

    scm_c_export (s_gmk_s_make_particle_system,
                  s_gmk_s_update_particle_system,
                  s_gmk_s_draw_particle_system,
                  s_gmk_s_particle_sprite_sheet,
                  s_gmk_s_particle_blend_additive,
                  s_gmk_s_max_particles,
                  s_gmk_s_particle_count,
                  s_gmk_s_particle_rate,
                  s_gmk_s_particle_amount,
                  s_gmk_s_particle_life,
                  s_gmk_s_particle_life_var,
                  s_gmk_s_particle_position,
                  s_gmk_s_particle_position_var,
                  s_gmk_s_particle_gravity,
                  s_gmk_s_particle_direction,
                  s_gmk_s_particle_direction_var,
                  s_gmk_s_particle_speed,
                  s_gmk_s_particle_speed_var,
                  s_gmk_s_particle_radial_accel,
                  s_gmk_s_particle_radial_accel_var,
                  s_gmk_s_particle_tan_accel,
                  s_gmk_s_particle_tan_accel_var,
                  s_gmk_s_particle_start_scale,
                  s_gmk_s_particle_start_scale_var,
                  s_gmk_s_particle_end_scale,
                  s_gmk_s_particle_end_scale_var,
                  s_gmk_s_particle_start_color,
                  s_gmk_s_particle_start_color_var,
                  s_gmk_s_particle_end_color,
                  s_gmk_s_particle_end_color_var,
                  s_gmk_s_set_particle_sprite_sheet,
                  s_gmk_s_set_particle_blend_additive,
                  s_gmk_s_set_particle_rate,
                  s_gmk_s_set_particle_amount,
                  s_gmk_s_set_particle_life,
                  s_gmk_s_set_particle_life_var,
                  s_gmk_s_set_particle_position,
                  s_gmk_s_set_particle_position_var,
                  s_gmk_s_set_particle_gravity,
                  s_gmk_s_set_particle_direction,
                  s_gmk_s_set_particle_direction_var,
                  s_gmk_s_set_particle_speed,
                  s_gmk_s_set_particle_speed_var,
                  s_gmk_s_set_particle_radial_accel,
                  s_gmk_s_set_particle_radial_accel_var,
                  s_gmk_s_set_particle_tan_accel,
                  s_gmk_s_set_particle_tan_accel_var,
                  s_gmk_s_set_particle_start_scale,
                  s_gmk_s_set_particle_start_scale_var,
                  s_gmk_s_set_particle_end_scale,
                  s_gmk_s_set_particle_end_scale_var,
                  s_gmk_s_set_particle_start_color,
                  s_gmk_s_set_particle_start_color_var,
                  s_gmk_s_set_particle_end_color,
                  s_gmk_s_set_particle_end_color_var,
                  NULL);
}
