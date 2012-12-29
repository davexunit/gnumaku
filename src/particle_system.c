#include "particle_system.h"

static scm_t_bits particle_system_tag;

ALLEGRO_COLOR
variable_color (ALLEGRO_COLOR color, ALLEGRO_COLOR color_var) {
    float r, b, g, a;
    float vr, vb, vg, va;

    al_unmap_rgba_f (color, &r, &g, &b, &a);
    al_unmap_rgba_f (color_var, &vr, &vg, &vb, &va);

    return al_map_rgba_f (r + vr * rand1 (),
                          g + vg * rand1 (),
                          b + vb * rand1 (),
                          a + va * rand1 ());
}

ALLEGRO_COLOR
delta_color (ALLEGRO_COLOR start_color, ALLEGRO_COLOR end_color, int duration) {
    float r1, b1, g1, a1;
    float r2, b2, g2, a2;

    al_unmap_rgba_f (start_color, &r1, &g1, &b1, &a1);
    al_unmap_rgba_f (end_color, &r2, &g2, &b2, &a2);

    return al_map_rgba_f ((r2 - r1) / duration,
                          (g2 - g1) / duration,
                          (b2 - b1) / duration,
                          (a2 - a1) / duration);
}

ALLEGRO_COLOR
add_color (ALLEGRO_COLOR a, ALLEGRO_COLOR b) {
    ALLEGRO_COLOR color;

    color.r = a.r + b.r;
    color.g = a.g + b.g;
    color.b = a.b + b.b;
    color.a = a.a + b.a;

    return color;
}

static ParticleSystem*
check_particle_system (SCM particle_system_smob) {
    scm_assert_smob_type (particle_system_tag, particle_system_smob);

    return (ParticleSystem *) SCM_SMOB_DATA (particle_system_smob);
}

static void
init_particle (ParticleSystem *system, Particle *particle) {
    ALLEGRO_COLOR start_color = variable_color (system->start_color, system->start_color_var);
    ALLEGRO_COLOR end_color = variable_color (system->end_color, system->end_color_var);
    SpriteSheet *sprite_sheet = check_sprite_sheet (system->sprite_sheet);
    int tile = (int) sprite_sheet->num_tiles * rand1 ();
    float theta = deg2rad (system->direction + system->direction_var * rand1 ());
    float speed = system->speed + system->speed_var * rand1 ();
    float start_scale = system->start_scale + system->start_scale_var * rand1 ();
    float end_scale = system->end_scale + system->end_scale_var * rand1 ();

    particle->life = (int) system->life + system->life_var * rand1 ();
    particle->image = sprite_sheet_tile (sprite_sheet, tile);
    particle->color = start_color;
    particle->dcolor = delta_color (start_color, end_color, particle->life);
    particle->pos.x = system->pos.x + system->pos_var.x * rand1 ();
    particle->pos.y = system->pos.y +  system->pos_var.y * rand1 ();
    particle->vel = vector2_from_polar (speed, theta);
    particle->radial_accel = system->radial_accel + system->radial_accel_var * rand1 ();
    particle->tan_accel = system->tan_accel + system->tan_accel_var * rand1 ();
    particle->scale = start_scale;
    particle->dscale = (end_scale - start_scale) / particle->life;
    particle->duration = 0;
    particle->active = true;
}
static SCM
make_particle_system (SCM s_max_particles, SCM sprite_sheet_smob)
{
    SCM smob;
    ParticleSystem *particle_system;
    int max_particles = scm_to_int (s_max_particles);

    /* Step 1: Allocate the memory block.
     */
    particle_system = (ParticleSystem *) scm_gc_malloc (sizeof (ParticleSystem), "particle_system");

    /* Step 2: Initialize it with straight code.
     */
    particle_system->max_particles = max_particles;
    particle_system->particle_count = 0;
    particle_system->blend_additive = true;
    particle_system->timer = 0;
    particle_system->rate = 0;
    particle_system->amount = 0;
    particle_system->life = 0;
    particle_system->life_var = 0;
    particle_system->pos.x = 0;
    particle_system->pos.y = 0;
    particle_system->pos_var.y = 0;
    particle_system->pos_var.x = 0;
    particle_system->radial_accel = 0;
    particle_system->radial_accel_var = 0;
    particle_system->tan_accel = 0;
    particle_system->tan_accel_var = 0;
    particle_system->direction = 0;
    particle_system->direction_var = 0;
    particle_system->speed = 0;
    particle_system->speed_var = 0;
    particle_system->start_scale = 1;
    particle_system->start_scale_var = 0;
    particle_system->end_scale = 1;
    particle_system->end_scale_var = 0;
    particle_system->start_color = al_map_rgba_f (1, 1, 1, 1);
    particle_system->start_color_var = al_map_rgba_f (0, 0, 0, 0);
    particle_system->end_color = al_map_rgba_f (0, 0, 0, 0);
    particle_system->end_color_var = al_map_rgba_f (0, 0, 0, 0);

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, particle_system_tag, particle_system);

    /* Step 4: Finish the initialization.
     */
    particle_system->particles = (Particle *) scm_gc_malloc (sizeof (Particle) * max_particles, "particles");
    particle_system->sprite_sheet = sprite_sheet_smob;

    return smob;
}

static SCM
mark_particle_system (SCM particle_system_smob)
{
    ParticleSystem *particle_system = (ParticleSystem *) SCM_SMOB_DATA (particle_system_smob);

    return particle_system->sprite_sheet;
}

static size_t
free_particle_system (SCM particle_system_smob)
{
    ParticleSystem *particle_system = (ParticleSystem *) SCM_SMOB_DATA (particle_system_smob);

    scm_gc_free (particle_system->particles,
                 sizeof (Particle) * particle_system->max_particles, "particles");
    scm_gc_free (particle_system, sizeof (ParticleSystem), "particle_system");

    return 0;
}

static int
print_particle_system (SCM particle_system_smob, SCM port, scm_print_state *pstate)
{
    ParticleSystem *particle_system = (ParticleSystem *) SCM_SMOB_DATA (particle_system_smob);

    scm_puts ("#<particle-system ", port);
    scm_display (scm_from_int(particle_system->max_particles), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

static void
draw_particle (Particle *particle) {
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

static SCM
draw_particle_system (SCM particle_system_smob) {
    ParticleSystem *system = check_particle_system (particle_system_smob);
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

static Particle*
get_free_particle (ParticleSystem *system) {
    if (system->particle_count < system->max_particles) {
        return &system->particles[system->particle_count++];
    }

    return NULL;
}

static void
free_particle (ParticleSystem *system, int index) {
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
particle_system_do_emit (ParticleSystem *system) {
    for (int i = 0; i < system->amount; ++i) {
        Particle *particle = get_free_particle (system);

        if (particle) {
            init_particle (system, particle);
       }
    }
}


static void
update_particle (ParticleSystem *system, int index) {
    Particle *particle = &system->particles[index];
    Vector2 radial = vector2_zero();
    Vector2 tangent;
    
    if (particle->active) {
        if (!vector2_equal (particle->pos, system->pos)) {
            radial = vector2_norm (vector2_sub (system->pos, particle->pos));
        }

        tangent = vector2_right_normal (radial);
        radial = vector2_scale (radial, particle->radial_accel);
        tangent = vector2_scale (tangent, particle->tan_accel);
        particle->vel = vector2_add (particle->vel, (vector2_add (radial, tangent)));
        particle->pos = vector2_add (particle->pos, particle->vel);
        particle->scale += particle->dscale;
        particle->color = add_color (particle->color, particle->dcolor);
        ++particle->duration;

        if (particle->duration >= particle->life) {
            free_particle (system, index);
        }
    }
}

static SCM
update_particle_system (SCM particle_system_smob) {
    ParticleSystem *system = check_particle_system (particle_system_smob);
    
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

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
particle_system_sprite_sheet (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return particle_system->sprite_sheet;
}

static SCM
particle_system_blend_additive (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_bool (particle_system->blend_additive);
}

static SCM
particle_system_max_particles (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->max_particles);
}

static SCM
particle_system_particle_count (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->particle_count);
}

static SCM
particle_system_rate (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->rate);
}

static SCM
particle_system_amount (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->amount);
}

static SCM
particle_system_life (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->life);
}

static SCM
particle_system_life_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_int (particle_system->life_var);
}

static SCM
particle_system_x (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->pos.x);
}

static SCM
particle_system_y (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->pos.y);
}

static SCM
particle_system_x_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->pos_var.x);
}

static SCM
particle_system_y_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->pos_var.y);
}

static SCM
particle_system_gravity_x (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->gravity.x);
}

static SCM
particle_system_gravity_y (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->gravity.y);
}

static SCM
particle_system_direction (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->direction);
}

static SCM
particle_system_direction_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->direction_var);
}

static SCM
particle_system_speed (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->speed);
}

static SCM
particle_system_speed_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->speed_var);
}

static SCM
particle_system_radial_accel (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->radial_accel);
}

static SCM
particle_system_radial_accel_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->radial_accel_var);
}

static SCM
particle_system_tan_accel (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->tan_accel);
}

static SCM
particle_system_tan_accel_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->tan_accel_var);
}

static SCM
particle_system_start_scale (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->start_scale);
}

static SCM
particle_system_start_scale_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->start_scale_var);
}

static SCM
particle_system_end_scale (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->end_scale);
}

static SCM
particle_system_end_scale_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_double (particle_system->end_scale_var);
}

static SCM
particle_system_start_color (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_color (particle_system->start_color);
}

static SCM
particle_system_start_color_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_color (particle_system->start_color_var);
}

static SCM
particle_system_end_color (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_color (particle_system->end_color);
}

static SCM
particle_system_end_color_var (SCM particle_system_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    scm_remember_upto_here_1 (particle_system_smob);

    return scm_from_color (particle_system->end_color_var);
}

static SCM
set_particle_system_sprite_sheet (SCM particle_system_smob, SCM sprite_sheet_smob) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);

    particle_system->sprite_sheet = sprite_sheet_smob;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_blend_additive (SCM particle_system_smob, SCM s_blend_additive) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    bool blend_additive = scm_to_bool (s_blend_additive);

    particle_system->blend_additive = blend_additive;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_rate (SCM particle_system_smob, SCM s_rate) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    int rate = scm_to_int (s_rate);

    particle_system->rate = rate;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_amount (SCM particle_system_smob, SCM s_amount) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    int amount = scm_to_int (s_amount);

    particle_system->amount = amount;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_life (SCM particle_system_smob, SCM s_life) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    int life = scm_to_int (s_life);

    particle_system->life = life;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_life_var (SCM particle_system_smob, SCM s_life_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    int life_var = scm_to_int (s_life_var);

    particle_system->life_var = life_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_x (SCM particle_system_smob, SCM s_x) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float x = scm_to_double (s_x);

    particle_system->pos.x = x;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_y (SCM particle_system_smob, SCM s_y) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float y = scm_to_double (s_y);

    particle_system->pos.y = y;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_x_var (SCM particle_system_smob, SCM s_x_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float x_var = scm_to_double (s_x_var);

    particle_system->pos_var.x = x_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_y_var (SCM particle_system_smob, SCM s_y_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float y_var = scm_to_double (s_y_var);

    particle_system->pos_var.y = y_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_gravity_x (SCM particle_system_smob, SCM s_gravity_x) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float gravity_x = scm_to_double (s_gravity_x);

    particle_system->gravity.x = gravity_x;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_gravity_y (SCM particle_system_smob, SCM s_gravity_y) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float gravity_y = scm_to_double (s_gravity_y);

    particle_system->gravity.y = gravity_y;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_direction (SCM particle_system_smob, SCM s_direction) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float direction = scm_to_double (s_direction);

    particle_system->direction = direction;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_direction_var (SCM particle_system_smob, SCM s_direction_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float direction_var = scm_to_double (s_direction_var);

    particle_system->direction_var = direction_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM set_particle_system_speed (SCM particle_system_smob, SCM s_speed) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float speed = scm_to_double (s_speed);

    particle_system->speed = speed;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_speed_var (SCM particle_system_smob, SCM s_speed_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float speed_var = scm_to_double (s_speed_var);

    particle_system->speed_var = speed_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_radial_accel (SCM particle_system_smob, SCM s_radial_accel) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float radial_accel = scm_to_double (s_radial_accel);

    particle_system->radial_accel = radial_accel;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_radial_accel_var (SCM particle_system_smob, SCM s_radial_accel_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float radial_accel_var = scm_to_double (s_radial_accel_var);

    particle_system->radial_accel_var = radial_accel_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_tan_accel (SCM particle_system_smob, SCM s_tan_accel) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float tan_accel = scm_to_double (s_tan_accel);

    particle_system->tan_accel = tan_accel;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_tan_accel_var (SCM particle_system_smob, SCM s_tan_accel_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float tan_accel_var = scm_to_double (s_tan_accel_var);

    particle_system->tan_accel_var = tan_accel_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_start_scale (SCM particle_system_smob, SCM s_start_scale) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float start_scale = scm_to_double (s_start_scale);

    particle_system->start_scale = start_scale;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_start_scale_var (SCM particle_system_smob, SCM s_start_scale_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float start_scale_var = scm_to_double (s_start_scale_var);

    particle_system->start_scale_var = start_scale_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_end_scale (SCM particle_system_smob, SCM s_end_scale) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float end_scale = scm_to_double (s_end_scale);

    particle_system->end_scale = end_scale;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_end_scale_var (SCM particle_system_smob, SCM s_end_scale_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    float end_scale_var = scm_to_double (s_end_scale_var);

    particle_system->end_scale_var = end_scale_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_start_color (SCM particle_system_smob, SCM s_start_color) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    ALLEGRO_COLOR start_color = scm_to_color (s_start_color);

    particle_system->start_color = start_color;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_start_color_var (SCM particle_system_smob, SCM s_start_color_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    ALLEGRO_COLOR start_color_var = scm_to_color (s_start_color_var);

    particle_system->start_color_var = start_color_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}


static SCM
set_particle_system_end_color (SCM particle_system_smob, SCM s_end_color) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    ALLEGRO_COLOR end_color = scm_to_color (s_end_color);

    particle_system->end_color = end_color;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_particle_system_end_color_var (SCM particle_system_smob, SCM s_end_color_var) {
    ParticleSystem *particle_system = check_particle_system (particle_system_smob);
    ALLEGRO_COLOR end_color_var = scm_to_color (s_end_color_var);

    particle_system->end_color_var = end_color_var;

    scm_remember_upto_here_1 (particle_system_smob);

    return SCM_UNSPECIFIED;
}

void
init_particle_system_type (void) {
    particle_system_tag = scm_make_smob_type ("<particle-system>",
                                              sizeof (ParticleSystem));
    scm_set_smob_mark (particle_system_tag, mark_particle_system);
    scm_set_smob_free (particle_system_tag, free_particle_system);
    scm_set_smob_print (particle_system_tag, print_particle_system);

    scm_c_define_gsubr ("make-particle-system", 2, 0, 0, make_particle_system);
    scm_c_define_gsubr ("update-particle-system", 1, 0, 0, update_particle_system);
    scm_c_define_gsubr ("draw-particle-system", 1, 0, 0, draw_particle_system);
    scm_c_define_gsubr ("particle-sprite-sheet", 1, 0, 0, particle_system_sprite_sheet);
    scm_c_define_gsubr ("particle-blend-additive", 1, 0, 0, particle_system_blend_additive);
    scm_c_define_gsubr ("particle-max-particles", 1, 0, 0, particle_system_max_particles);
    scm_c_define_gsubr ("particle-count", 1, 0, 0, particle_system_particle_count);
    scm_c_define_gsubr ("particle-rate", 1, 0, 0, particle_system_rate);
    scm_c_define_gsubr ("particle-amount", 1, 0, 0, particle_system_amount);
    scm_c_define_gsubr ("particle-life", 1, 0, 0, particle_system_life);
    scm_c_define_gsubr ("particle-life-var", 1, 0, 0, particle_system_life_var);
    scm_c_define_gsubr ("particle-x", 1, 0, 0, particle_system_x);
    scm_c_define_gsubr ("particle-y", 1, 0, 0, particle_system_y);
    scm_c_define_gsubr ("particle-x-var", 1, 0, 0, particle_system_x_var);
    scm_c_define_gsubr ("particle-y-var", 1, 0, 0, particle_system_y_var);
    scm_c_define_gsubr ("particle-gravity-x", 1, 0, 0, particle_system_gravity_x);
    scm_c_define_gsubr ("particle-gravity-y", 1, 0, 0, particle_system_gravity_y);
    scm_c_define_gsubr ("particle-direction", 1, 0, 0, particle_system_direction);
    scm_c_define_gsubr ("particle-direction-var", 1, 0, 0, particle_system_direction_var);
    scm_c_define_gsubr ("particle-speed", 1, 0, 0, particle_system_speed);
    scm_c_define_gsubr ("particle-speed-var", 1, 0, 0, particle_system_speed_var);
    scm_c_define_gsubr ("particle-radial-accel", 1, 0, 0, particle_system_radial_accel);
    scm_c_define_gsubr ("particle-radial-accel-var", 1, 0, 0, particle_system_radial_accel_var);
    scm_c_define_gsubr ("particle-tan-accel", 1, 0, 0, particle_system_tan_accel);
    scm_c_define_gsubr ("particle-tan-accel-var", 1, 0, 0, particle_system_tan_accel_var);
    scm_c_define_gsubr ("particle-start-scale", 1, 0, 0, particle_system_start_scale);
    scm_c_define_gsubr ("particle-start-scale-var", 1, 0, 0, particle_system_start_scale_var);
    scm_c_define_gsubr ("particle-end-scale", 1, 0, 0, particle_system_end_scale);
    scm_c_define_gsubr ("particle-end-scale-var", 1, 0, 0, particle_system_end_scale_var);
    scm_c_define_gsubr ("particle-start-color", 1, 0, 0, particle_system_start_color);
    scm_c_define_gsubr ("particle-start-color-var", 1, 0, 0, particle_system_start_color_var);
    scm_c_define_gsubr ("particle-end-color", 1, 0, 0, particle_system_end_color);
    scm_c_define_gsubr ("particle-end-color-var", 1, 0, 0, particle_system_end_color_var);
    scm_c_define_gsubr ("set-particle-sprite-sheet", 2, 0, 0, set_particle_system_sprite_sheet);
    scm_c_define_gsubr ("set-particle-blend-additive", 2, 0, 0, set_particle_system_blend_additive);
    scm_c_define_gsubr ("set-particle-rate", 2, 0, 0, set_particle_system_rate);
    scm_c_define_gsubr ("set-particle-amount", 2, 0, 0, set_particle_system_amount);
    scm_c_define_gsubr ("set-particle-life", 2, 0, 0, set_particle_system_life);
    scm_c_define_gsubr ("set-particle-life-var", 2, 0, 0, set_particle_system_life_var);
    scm_c_define_gsubr ("set-particle-x", 2, 0, 0, set_particle_system_x);
    scm_c_define_gsubr ("set-particle-y", 2, 0, 0, set_particle_system_y);
    scm_c_define_gsubr ("set-particle-x-var", 2, 0, 0, set_particle_system_x_var);
    scm_c_define_gsubr ("set-particle-y-var", 2, 0, 0, set_particle_system_y_var);
    scm_c_define_gsubr ("set-particle-gravity-x", 2, 0, 0, set_particle_system_gravity_x);
    scm_c_define_gsubr ("set-particle-gravity-y", 2, 0, 0, set_particle_system_gravity_y);
    scm_c_define_gsubr ("set-particle-direction", 2, 0, 0, set_particle_system_direction);
    scm_c_define_gsubr ("set-particle-direction-var", 2, 0, 0, set_particle_system_direction_var);
    scm_c_define_gsubr ("set-particle-speed", 2, 0, 0, set_particle_system_speed);
    scm_c_define_gsubr ("set-particle-speed-var", 2, 0, 0, set_particle_system_speed_var);
    scm_c_define_gsubr ("set-particle-radial-accel", 2, 0, 0, set_particle_system_radial_accel);
    scm_c_define_gsubr ("set-particle-radial-accel-var", 2, 0, 0, set_particle_system_radial_accel_var);
    scm_c_define_gsubr ("set-particle-tan-accel", 2, 0, 0, set_particle_system_tan_accel);
    scm_c_define_gsubr ("set-particle-tan-accel-var", 2, 0, 0, set_particle_system_tan_accel_var);
    scm_c_define_gsubr ("set-particle-start-scale", 2, 0, 0, set_particle_system_start_scale);
    scm_c_define_gsubr ("set-particle-start-scale-var", 2, 0, 0, set_particle_system_start_scale_var);
    scm_c_define_gsubr ("set-particle-end-scale", 2, 0, 0, set_particle_system_end_scale);
    scm_c_define_gsubr ("set-particle-end-scale-var", 2, 0, 0, set_particle_system_end_scale_var);
    scm_c_define_gsubr ("set-particle-start-color", 2, 0, 0, set_particle_system_start_color);
    scm_c_define_gsubr ("set-particle-start-color-var", 2, 0, 0, set_particle_system_start_color_var);
    scm_c_define_gsubr ("set-particle-end-color", 2, 0, 0, set_particle_system_end_color);
    scm_c_define_gsubr ("set-particle-end-color-var", 2, 0, 0, set_particle_system_end_color_var);

    scm_c_export ("make-particle-system", NULL);
    scm_c_export ("update-particle-system", NULL);
    scm_c_export ("draw-particle-system", NULL);
    scm_c_export ("particle-sprite-sheet", NULL);
    scm_c_export ("particle-blend-additive", NULL);
    scm_c_export ("particle-max-particles", NULL);
    scm_c_export ("particle-count", NULL);
    scm_c_export ("particle-rate", NULL);
    scm_c_export ("particle-amount", NULL);
    scm_c_export ("particle-life", NULL);
    scm_c_export ("particle-life-var", NULL);
    scm_c_export ("particle-x", NULL);
    scm_c_export ("particle-y", NULL);
    scm_c_export ("particle-x-var", NULL);
    scm_c_export ("particle-y-var", NULL);
    scm_c_export ("particle-gravity-x", NULL);
    scm_c_export ("particle-gravity-y", NULL);
    scm_c_export ("particle-direction", NULL);
    scm_c_export ("particle-direction-var", NULL);
    scm_c_export ("particle-speed", NULL);
    scm_c_export ("particle-speed-var", NULL);
    scm_c_export ("particle-radial-accel", NULL);
    scm_c_export ("particle-radial-accel-var", NULL);
    scm_c_export ("particle-tan-accel", NULL);
    scm_c_export ("particle-tan-accel-var", NULL);
    scm_c_export ("particle-start-scale", NULL);
    scm_c_export ("particle-start-scale-var", NULL);
    scm_c_export ("particle-end-scale", NULL);
    scm_c_export ("particle-end-scale-var", NULL);
    scm_c_export ("particle-start-color", NULL);
    scm_c_export ("particle-start-color-var", NULL);
    scm_c_export ("particle-end-color", NULL);
    scm_c_export ("particle-end-color-var", NULL);
    scm_c_export ("set-particle-sprite-sheet", NULL);
    scm_c_export ("set-particle-blend-additive", NULL);
    scm_c_export ("set-particle-rate", NULL);
    scm_c_export ("set-particle-amount", NULL);
    scm_c_export ("set-particle-life", NULL);
    scm_c_export ("set-particle-life-var", NULL);
    scm_c_export ("set-particle-x", NULL);
    scm_c_export ("set-particle-y", NULL);
    scm_c_export ("set-particle-x-var", NULL);
    scm_c_export ("set-particle-y-var", NULL);
    scm_c_export ("set-particle-gravity-x", NULL);
    scm_c_export ("set-particle-gravity-y", NULL);
    scm_c_export ("set-particle-direction", NULL);
    scm_c_export ("set-particle-direction-var", NULL);
    scm_c_export ("set-particle-speed", NULL);
    scm_c_export ("set-particle-speed-var", NULL);
    scm_c_export ("set-particle-radial-accel", NULL);
    scm_c_export ("set-particle-radial-accel-var", NULL);
    scm_c_export ("set-particle-tan-accel", NULL);
    scm_c_export ("set-particle-tan-accel-var", NULL);
    scm_c_export ("set-particle-start-scale", NULL);
    scm_c_export ("set-particle-start-scale-var", NULL);
    scm_c_export ("set-particle-end-scale", NULL);
    scm_c_export ("set-particle-end-scale-var", NULL);
    scm_c_export ("set-particle-start-color", NULL);
    scm_c_export ("set-particle-start-color-var", NULL);
    scm_c_export ("set-particle-end-color", NULL);
    scm_c_export ("set-particle-end-color-var", NULL);
}
