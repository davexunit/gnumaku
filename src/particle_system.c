#include "particle_system.h"
#include "math.h"

static scm_t_bits particle_system_tag;

/* Keywords for the %emit-bullet procedure. */
SCM_KEYWORD (kw_accel, "accel");
SCM_KEYWORD (kw_ang_vel, "ang-vel");
SCM_KEYWORD (kw_lifetime, "lifetime");
SCM_KEYWORD (kw_color, "color");
SCM_KEYWORD (kw_directional, "directional");
SCM_KEYWORD (kw_data, "data");

/*
 * Assert that the SCM object is indeed a particle system and return a
 * pointer to the GmkParticleSystem.
 */
static GmkParticleSystem *
check_particle_system (SCM particle_system)
{
    scm_assert_smob_type (particle_system_tag, particle_system);

    return (GmkParticleSystem *) SCM_SMOB_DATA (particle_system);
}

/* Helper functions for determining the size of particle arrays. */
static int
particles_size (GmkParticleSystem *particle_system)
{
    return sizeof (GmkParticle) * particle_system->max_particles;
}

static int
particle_ids_size (GmkParticleSystem *particle_system)
{
    return sizeof (int) * particle_system->max_particles;
}

static GmkParticle *
make_particles (GmkParticleSystem *particle_system)
{
    return (GmkParticle *) scm_gc_malloc (particles_size (particle_system),
                                          "particles");
}

static int *
make_particle_ids (GmkParticleSystem *particle_system)
{
    return (int *) scm_gc_malloc (particle_ids_size (particle_system),
                                  "particle ids");
}

static void
update_particle_body (GmkParticleBody *body)
{
    al_transform_coordinates (&body->ang_vel,
                              &body->vel.x, &body->vel.y);
    al_transform_coordinates (&body->ang_vel,
                              &body->accel.x, &body->accel.y);
    body->pos = gmk_vector2_add (body->pos, body->vel);
    body->vel = gmk_vector2_add (body->vel, body->accel);
}

static bool
is_particle_time_expired (GmkParticle *particle)
{
    /* max_lifetime of 0 means unlimited life span. */
    return particle->max_lifetime != 0 && particle->lifetime >= particle->max_lifetime;
}

static bool
is_particle_dead (GmkParticle *particle)
{
    return particle->kill || is_particle_time_expired (particle);
}

static void
update_particle (GmkParticle *particle)
{
    update_particle_body (&particle->body);
    particle->lifetime++;
    particle->kill = is_particle_dead (particle);
}

static float
particle_sprite_angle (GmkParticle *particle)
{
    if (particle->directional) {
        return gmk_vector2_angle (particle->body.vel);
    }

    return 0;
}

static void
draw_particle (GmkParticle *particle)
{
    GmkParticleBody body = particle->body;
    float cx = al_get_bitmap_width (particle->bitmap) / 2;
    float cy = al_get_bitmap_height (particle->bitmap) / 2;
    float angle = particle_sprite_angle (particle);
    // ALLEGRO_COLOR color = gmk_color_mult_alpha (particle->color);

    al_draw_tinted_scaled_rotated_bitmap (particle->bitmap, particle->color,
                                          cx, cy,
                                          body.pos.x, body.pos.y,
                                          body.scale.x, body.scale.y,
                                          angle, 0);
}

static void
free_particle (GmkParticleSystem *particle_system, int index)
{
    GmkParticle *particle = particle_system->particles + index;
    GmkParticle temp;
    int particle_count = --particle_system->particle_count;
    int particle_id = particle->id;

    /* Swap particles in memory pool. */
    temp = particle_system->particles[particle_count];
    particle_system->particles[particle_count] = *particle;
    particle_system->particles[index] = temp;

    /* Update id->index mapping. */
    particle_system->particle_ids[temp.id] = index;
    particle_system->particle_ids[particle_id] = particle_count;
}

SCM_DEFINE (gmk_make_particle_system, "make-particle-system", 1, 0, 0,
            (SCM max_particles),
            "Makes a new particle system and allocates memory for "
            "@var{max_particles} particles.")
{
    SCM smob;
    GmkParticleSystem *particle_system;

    particle_system = (GmkParticleSystem *) scm_gc_malloc (sizeof (GmkParticleSystem),
                                                           "particle system");
    particle_system->max_particles = scm_to_int (max_particles);
    particle_system->particle_count = 0;
    particle_system->particles = NULL;
    particle_system->particle_ids = NULL;
    SCM_NEWSMOB (smob, particle_system_tag, particle_system);
    particle_system->particles = make_particles (particle_system);
    particle_system->particle_ids = make_particle_ids (particle_system);

    return smob;
}

SCM_DEFINE (gmk_particle_system_max, "particle-system-max", 1, 0, 0,
            (SCM particle_system),
            "Returns the maximum number of particles that can be in the system "
            "at once.")
{
    GmkParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->max_particles);
}

SCM_DEFINE (gmk_particle_system_count, "particle-system-count", 1, 0, 0,
            (SCM particle_system),
            "Returns the number of active particles that are in the system.")
{
    GmkParticleSystem *system = check_particle_system (particle_system);

    return scm_from_int (system->particle_count);
}

SCM_DEFINE (gmk_update_particle_system, "update-particle-system!", 1, 0, 0,
            (SCM particle_system),
            "Update particle system.")
{
    GmkParticleSystem *system = check_particle_system (particle_system);

    for (int i = 0; i < system->particle_count; ++i) {
        GmkParticle *particle = system->particles + i;

        update_particle (particle);

        if (particle->kill) {
            free_particle (system, i--);
        }
    }

    scm_remember_upto_here_1 (particle_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_draw_particle_system, "draw-particle-system", 1, 0, 0,
            (SCM particle_system),
            "Draw particle system.")
{
    GmkParticleSystem *system = check_particle_system (particle_system);
    ALLEGRO_STATE state;

    al_store_state (&state, ALLEGRO_STATE_BLENDER);
    al_hold_bitmap_drawing (true);

    for (int i = 0; i < system->particle_count; ++i) {
        GmkParticle *particle = system->particles + i;

        draw_particle (particle);
    }

    al_hold_bitmap_drawing (false);
    al_restore_state (&state);
    scm_remember_upto_here_1 (particle_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_emit_particle, "%emit-particle!", 5, 0, 1,
            (SCM particle_system, SCM pos, SCM speed, SCM direction,
             SCM bitmap, SCM kwargs),
            "Creates a new particle and adds it to the system")
{
    GmkParticleSystem *system = check_particle_system (particle_system);
    GmkParticle particle;
    GmkParticleBody body;
    double theta = gmk_deg_to_rad (scm_to_double (direction));
    /*
     * Using keyword arguments for properties that aren't always
     * needed, like angular velocity.
     */
    SCM accel = scm_get_keyword (kw_accel, kwargs, scm_from_double (0));
    SCM ang_vel = scm_get_keyword (kw_ang_vel, kwargs, scm_from_double (0));
    SCM max_lifetime = scm_get_keyword (kw_lifetime, kwargs, scm_from_int (0));
    SCM directional = scm_get_keyword (kw_directional, kwargs, SCM_BOOL_F);
    SCM data = scm_get_keyword (kw_data, kwargs, SCM_BOOL_F);

    /* Bail out if we can't create any more particles. */
    if (system->particle_count == system->max_particles) {
        return SCM_BOOL_F;
    }

    body.pos.x = scm_to_double (scm_car (pos));
    body.pos.y = scm_to_double (scm_cadr (pos));
    body.scale.x = 1;
    body.scale.y = 1;
    body.vel = gmk_vector2_from_polar (scm_to_double (speed), theta);
    body.accel = gmk_vector2_from_polar (scm_to_double (accel), theta);
    body.hitbox = gmk_rect_new (-4, -4, 8, 8);
    /* Angular velocity is stored as a transformation matrix. */
    al_build_transform (&body.ang_vel, 0, 0, 1, 1,
                        gmk_deg_to_rad (scm_to_double (ang_vel)));
    particle.body = body;
    particle.kill = false;
    particle.directional = scm_to_bool (directional);
    particle.max_lifetime = scm_to_int (max_lifetime);
    particle.lifetime = 0;
    particle.bitmap = scm_to_pointer (bitmap);
    particle.color = al_map_rgba_f (1, 1, 1, 1);
    particle.data = data;
    system->particles[system->particle_count++] = particle;

    return SCM_BOOL_T;
}

SCM_DEFINE (gmk_clear_particle_system, "clear-particle-system!", 1, 0, 0,
            (SCM particle_system),
            "Removes all particles from the particle system.")
{
    GmkParticleSystem *system = check_particle_system (particle_system);

    system->particle_count = 0;
    scm_remember_upto_here_1 (particle_system);

    return SCM_UNSPECIFIED;
}

static SCM
mark_particle_system (SCM particle_system)
{
    // GmkParticleSystem *system = (GmkParticleSystem *) SCM_SMOB_DATA (particle_system);

    return SCM_BOOL_F;
}

static size_t
free_particle_system (SCM particle_system)
{
    GmkParticleSystem *system = (GmkParticleSystem *) SCM_SMOB_DATA (particle_system);

    scm_gc_free (system->particles, particles_size (system), "particles");
    scm_gc_free (system->particle_ids, particle_ids_size (system), "particle ids");
    scm_gc_free (system, sizeof (GmkParticleSystem), "particle system");

    return 0;
}

static int
print_particle_system (SCM particle_system, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<particle-system max-particles: ", port);
    // scm_display (gmk_s_max_particles (particle_system), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_particle_system (void)
{
    particle_system_tag = scm_make_smob_type ("<particle-system>", sizeof (GmkParticleSystem));
    scm_set_smob_mark (particle_system_tag, mark_particle_system);
    scm_set_smob_free (particle_system_tag, free_particle_system);
    scm_set_smob_print (particle_system_tag, print_particle_system);

#include "particle_system.x"

    scm_c_export (s_gmk_make_particle_system,
                  s_gmk_particle_system_max,
                  s_gmk_particle_system_count,
                  s_gmk_update_particle_system,
                  s_gmk_draw_particle_system,
                  s_gmk_emit_particle,
                  s_gmk_clear_particle_system,
                  NULL);
}
