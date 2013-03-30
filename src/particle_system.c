#include "particle_system.h"

static scm_t_bits particle_system_tag;

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

SCM_DEFINE (gmk_s_make_particle_system, "make-particle-system", 1, 0, 0,
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

static SCM
mark_particle_system (SCM particle_system)
{
    GmkParticleSystem *system = (GmkParticleSystem *) SCM_SMOB_DATA (particle_system);

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

    scm_c_export (s_gmk_s_make_particle_system,
                  NULL);
}
