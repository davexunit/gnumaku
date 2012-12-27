#include "bullet_system.h"

static scm_t_bits bullet_system_tag;
static scm_t_bits bullet_ref_tag;
static scm_t_bits bullet_type_tag;

static BlendMode
scm_to_blend_mode (SCM blend_mode) {
    if (scm_eq_p (blend_mode, SYM_BLEND_ALPHA)) {
        return BLEND_ALPHA;
    } else if (scm_eq_p (blend_mode, SYM_BLEND_ADD)) {
        return BLEND_ADD;
    }

    return BLEND_ALPHA;
}

static SCM
make_bullet_type (SCM s_image, SCM s_hitbox, SCM s_blend_mode, SCM s_directional) {
    SCM smob;
    BulletType *bullet_type;
    int image = scm_to_int (s_image);
    Rect hitbox = scm_to_rect (s_hitbox);
    BlendMode blend_mode = scm_to_blend_mode (s_blend_mode);
    bool directional = scm_to_bool (s_directional);

    bullet_type = (BulletType *) scm_gc_malloc (sizeof (BulletType), "bullet_type");
    bullet_type->image = image;
    bullet_type->hitbox = hitbox;
    bullet_type->blend_mode = blend_mode;
    bullet_type->directional = directional;
    SCM_NEWSMOB (smob, bullet_type_tag, bullet_type);

    return smob;
}

static BulletType*
check_bullet_type (SCM bullet_type_smob) {
    scm_assert_smob_type (bullet_type_tag, bullet_type_smob);

    return (BulletType *) SCM_SMOB_DATA (bullet_type_smob);
}

static size_t
free_bullet_type (SCM bullet_type_smob) {
    BulletType *bullet_type = (BulletType *) SCM_SMOB_DATA (bullet_type_smob);

    scm_gc_free (bullet_type, sizeof (BulletType), "bullet_type");

    return 0;
}

static int
print_bullet_type (SCM bullet_type_smob, SCM port, scm_print_state *pstate) {
    scm_puts ("#<bullet-type>", port);

    /* non-zero means success */
    return 1;
}

static BulletSystem*
check_bullet_system (SCM bullet_system_smob) {
    scm_assert_smob_type (bullet_system_tag, bullet_system_smob);

    return (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);
}

static SCM make_bullet_ref (BulletSystem *bullet_system, int id);

static SCM
make_bullet_system (SCM s_max_bullets, SCM sprite_sheet_smob) {
    SCM smob;
    BulletSystem *bullet_system;
    int max_bullets = scm_to_int (s_max_bullets);

    bullet_system = (BulletSystem *) scm_gc_malloc (sizeof (BulletSystem), "bullet_system");

    bullet_system->max_bullets = max_bullets;
    bullet_system->bullet_count = 0;
    bullet_system->bullets = NULL;
    bullet_system->sprite_sheet = SCM_BOOL_F;
    SCM_NEWSMOB (smob, bullet_system_tag, bullet_system);
    bullet_system->bullets = (Bullet *) scm_gc_malloc (sizeof (Bullet) * max_bullets,
                                                       "bullets");
    bullet_system->bullet_ids = (int *) scm_gc_malloc (sizeof (int) * max_bullets,
                                                       "bullet_ids");
    bullet_system->sprite_sheet = sprite_sheet_smob;

    /* Initialize bullets. */
    for (int i = 0; i < max_bullets; ++i) {
        Bullet *bullet = bullet_system->bullets + i;

        bullet->id = i;
        bullet->ref = make_bullet_ref (bullet_system, i);
        bullet->active = false;
        bullet_system->bullet_ids[i] = i;
    }

    return smob;
}

static SCM
mark_bullet_system (SCM bullet_system_smob) {
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    return bullet_system->sprite_sheet;
}

static size_t
free_bullet_system (SCM bullet_system_smob) {
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    scm_gc_free (bullet_system->bullets, sizeof (Bullet) * bullet_system->max_bullets,
                 "bullets");
    scm_gc_free (bullet_system->bullet_ids, sizeof (int) * bullet_system->max_bullets,
                 "bullet_ids");
    scm_gc_free (bullet_system, sizeof (BulletSystem), "bullet_system");

    return 0;
}

static int
print_bullet_system (SCM bullet_system_smob, SCM port, scm_print_state *pstate) {
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    scm_puts ("#<BulletSystem ", port);
    scm_display (scm_from_int(bullet_system->max_bullets), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

static SCM
clear_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    bullet_system->bullet_count = 0;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static bool
bullet_out_of_bounds(BulletSystem *bullet_system, Bullet *bullet) {
    return !rect_collide_point(&bullet_system->bounds, bullet->x, bullet->y);
}

static int
bullet_index_from_id (BulletSystem *bullet_system, int id) {
    return bullet_system->bullet_ids[id];
}

static Bullet*
bullet_from_id (BulletSystem *bullet_system, int id) {
    int index = bullet_index_from_id (bullet_system, id);
    
    return bullet_system->bullets + index;
}

static void
free_bullet (BulletSystem *bullet_system, int index) {
    Bullet *bullet = bullet_system->bullets + index;
    Bullet temp;
    int bullet_count = --bullet_system->bullet_count;
    int bullet_id = bullet->id;

    bullet->active = false;

    /* Swap bullets in memory pool. */
    temp = bullet_system->bullets[bullet_count];
    bullet_system->bullets[bullet_count] = *bullet;
    bullet_system->bullets[index] = temp;

    /* Update id->index mapping. */
    bullet_system->bullet_ids[temp.id] = index;
    bullet_system->bullet_ids[bullet_id] = bullet_count;
}

static SCM
set_bullet_system_bounds (SCM bullet_system_smob, SCM s_x, SCM s_y,
                          SCM s_width, SCM s_height) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float width = scm_to_double (s_width);
    float height = scm_to_double (s_height);

    bullet_system->bounds.x = x;
    bullet_system->bounds.y = y;
    bullet_system->bounds.width = width;
    bullet_system->bounds.height = height;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
bullet_system_bounds (SCM bullet_system_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    Rect bounds = bullet_system->bounds;
    SCM x = scm_from_double (bounds.x);
    SCM y = scm_from_double (bounds.y);
    SCM width = scm_from_double (bounds.width);
    SCM height = scm_from_double (bounds.height);

    scm_remember_upto_here_1 (bullet_system_smob);

    return make_rect(x, y, width, height);
}

static bool
bullet_has_script (Bullet *bullet) {
    return scm_to_bool (scm_procedure_p (bullet->script));
}

static void
update_bullet_script (Bullet *bullet) {
    /* If there is a script to run and it's time to run it. */
    if (bullet->life_count == bullet->script_time && bullet_has_script (bullet)) {
        scm_call_1 (bullet->script, bullet->ref);
    }
}

static void
update_bullet(Bullet *bullet) {
    update_bullet_script (bullet);
    al_transform_coordinates (&bullet->angular_velocity, &bullet->dx, &bullet->dy);
    al_transform_coordinates (&bullet->angular_velocity, &bullet->ddx, &bullet->ddy);
    bullet->x += bullet->dx;
    bullet->y += bullet->dy;
    bullet->dx += bullet->ddx;
    bullet->dy += bullet->ddy;
    bullet->life_count++;
}

static bool
bullet_has_life_remaining (Bullet *bullet) {
    /* Life span 0 means unlimited lifetime. */
    if (bullet->life == 0) {
        return true;
    }

    return bullet->life_count < bullet->life;
}

static bool
bullet_dead (BulletSystem *bullet_system, Bullet *bullet) {
    return (bullet->kill ||
            bullet_out_of_bounds (bullet_system, bullet) ||
            !bullet_has_life_remaining (bullet));
}

static SCM
update_bullet_system (SCM bullet_system_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    for (int i = 0; i < bullet_system->bullet_count; ++i) {
        Bullet *bullet = &bullet_system->bullets[i];

        if (bullet->active) {
            update_bullet (bullet);

            /* Remove bullets that are outside of the game field or have no lifetime
             * remaining. Only unscripted bullets are removed. */
            if (bullet_dead (bullet_system, bullet)) {
                free_bullet (bullet_system, i);
                --i;
            }
        }
    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static void
set_bullet_blend_mode (Bullet *bullet) {
    switch (bullet->blend_mode) {
    case BLEND_ALPHA:
        al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
        break;
    case BLEND_ADD:
        al_set_blender(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
        break;
    }
}

static float
bullet_sprite_angle (Bullet *bullet) {
    if (bullet->directional) {
        return atan2 (bullet->dy, bullet->dx);
    }

    return 0;
}

static void
draw_bullet (Bullet *bullet, int cx, int cy) {
    float angle = bullet_sprite_angle (bullet);

    set_bullet_blend_mode (bullet);
    al_draw_rotated_bitmap (bullet->image, cx, cy,
                             bullet->x, bullet->y, angle, 0);
}

static SCM
draw_bullet_system (SCM bullet_system_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet (bullet_system->sprite_sheet);

    ALLEGRO_STATE state;

    al_store_state (&state, ALLEGRO_STATE_BLENDER);
    al_hold_bitmap_drawing (true);

    for (int i = 0; i < bullet_system->bullet_count; ++i) {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->active && bullet->image) {
            draw_bullet (bullet, sprite_sheet->tile_width / 2,
                         sprite_sheet->tile_height / 2);
        }
    }
    
    al_hold_bitmap_drawing (false);
    al_restore_state (&state);
    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
draw_bullet_system_hitboxes (SCM bullet_system_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    for (int i = 0; i < bullet_system->bullet_count; ++i) {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->active && bullet->image) {
            Rect hitbox = rect_move(&bullet->hitbox, bullet->x, bullet->y);
            al_draw_rectangle (hitbox.x, hitbox.y, hitbox.x + hitbox.width, hitbox.y + hitbox.height,
                               al_map_rgba_f (1, 0, 1, 1), 2);
        }
    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static bool
bullet_collision_check (Bullet *bullet, Rect *rect, SCM callback) {
    Rect hitbox = rect_move(&bullet->hitbox, bullet->x, bullet->y);

    if (rect_collide_rect (&hitbox, rect)) {
        if (scm_procedure_p (callback)) {
            /* The callback can return true if the bullet should be removed from the system */
            return scm_to_bool (scm_call_0 (callback));
        }
    }

    return false;
}

static SCM
bullet_system_collide_rect (SCM bullet_system_smob, SCM rect_smob, SCM callback) {
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    Rect *rect = check_rect (rect_smob);

    for (int i = 0; i < bullet_system->bullet_count; ++i) {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->active && bullet_collision_check (bullet, rect, callback)) {
            bullet->active = false;
        }
    }

    return SCM_UNSPECIFIED;
}

static SCM
get_bullet_system_sprite_sheet (SCM bullet_system_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    scm_remember_upto_here_1 (bullet_system_smob);

    return bullet_system->sprite_sheet;
}

static SCM
set_bullet_system_sprite_sheet (SCM bullet_system_smob, SCM sprite_sheet_smob) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    bullet_system->sprite_sheet = sprite_sheet_smob;
    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static void
init_bullet_movement (Bullet *bullet, float speed, float direction, float acceleration,
                      float angular_velocity) {
    float dx = cos (direction);
    float dy = sin (direction);

    bullet->dx = dx * speed;
    bullet->dy = dy * speed;
    bullet->ddx = dx * acceleration;
    bullet->ddy = dy * acceleration;
    al_build_transform (&bullet->angular_velocity, 0, 0, 1, 1,
                        deg2rad (angular_velocity));
}

static void
init_bullet (Bullet *bullet, BulletSystem *bullet_system, float x, float y,
             float speed, float direction, float acceleration, float angular_velocity,
             int life, SCM script, BulletType *type) {
    SpriteSheet *sprite_sheet = check_sprite_sheet (bullet_system->sprite_sheet);

    bullet->active = true;
    bullet->kill = false;
    bullet->directional = type->directional;
    bullet->life = life;
    bullet->script_time = 0;
    bullet->life_count = 0;
    bullet->x = x;
    bullet->y = y;
    bullet->image = sprite_sheet_tile (sprite_sheet, type->image);
    bullet->color = al_map_rgba_f (1, 1, 1, 1);
    bullet->blend_mode = type->blend_mode;
    bullet->script = script;
    bullet->hitbox = type->hitbox;
    init_bullet_movement (bullet, speed, direction, acceleration, angular_velocity);
}

static SCM
make_bullet_ref (BulletSystem *bullet_system, int id) {
    SCM smob;
    BulletRef *bullet_ref;

    /* Error handling for out of range bullet ids */
    if (id < 0 || id >= bullet_system->max_bullets) {
        return SCM_BOOL_F;
    }

    bullet_ref = (BulletRef *) scm_gc_malloc (sizeof (BulletRef), "bullet_ref");
    bullet_ref->system = bullet_system;
    bullet_ref->id = id;
    SCM_NEWSMOB (smob, bullet_ref_tag, bullet_ref);

    return smob;
}

static BulletRef*
check_bullet_ref (SCM bullet_ref_smob) {
    scm_assert_smob_type (bullet_ref_tag, bullet_ref_smob);

    return (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);
}

static size_t
free_bullet_ref (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);

    scm_gc_free (bullet_ref, sizeof (BulletRef), "bullet_ref");

    return 0;
}

static int
print_bullet_ref (SCM bullet_ref_smob, SCM port, scm_print_state *pstate) {
    BulletRef *bullet_ref = (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);

    scm_puts ("#<BulletRef ", port);
    scm_display (scm_from_double (bullet_ref->id), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

static SCM
emit_bullet (SCM bullet_system_smob, SCM s_x, SCM s_y, SCM s_speed,
             SCM s_direction, SCM s_acceleration, SCM s_angular_velocity,
             SCM s_life, SCM s_type) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float speed = scm_to_double (s_speed);
    float direction = deg2rad (scm_to_double (s_direction));
    float acceleration = scm_to_double (s_acceleration);
    float angular_velocity = scm_to_double (s_angular_velocity);
    int life = scm_to_int (s_life);
    BulletType *type = check_bullet_type (s_type);
    Bullet *bullet;
    int index;

    if (bullet_system->bullet_count >= bullet_system->max_bullets) {
        return scm_from_int (-1);
    }

    /* Get new bullet from pool. */
    index = bullet_system->bullet_count++;
    bullet = &bullet_system->bullets[index];
    init_bullet (bullet, bullet_system, x, y, speed, direction, acceleration,
                 angular_velocity, life, SCM_BOOL_F, type);

    return SCM_UNSPECIFIED;
}

static SCM
emit_simple_bullet (SCM bullet_system_smob, SCM s_x, SCM s_y, SCM s_speed,
                    SCM s_direction, SCM s_type) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float speed = scm_to_double (s_speed);
    float direction = deg2rad (scm_to_double (s_direction));
    float acceleration = 0;
    float angular_velocity = 0;
    int life = 0;
    BulletType *type = check_bullet_type (s_type);
    Bullet *bullet;
    int index;

    if (bullet_system->bullet_count >= bullet_system->max_bullets) {
        return scm_from_int (-1);
    }

    /* Get new bullet from pool. */
    index = bullet_system->bullet_count++;
    bullet = &bullet_system->bullets[index];
    init_bullet (bullet, bullet_system, x, y, speed, direction, acceleration,
                 angular_velocity, life, SCM_BOOL_F, type);

    return SCM_UNSPECIFIED;
}

static SCM
emit_script_bullet (SCM bullet_system_smob, SCM s_x, SCM s_y, SCM s_type, SCM script) {
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    BulletType *type = check_bullet_type (s_type);
    Bullet *bullet;
    int index;

    if (bullet_system->bullet_count >= bullet_system->max_bullets) {
        return scm_from_int (-1);
    }

    /* Get new bullet from pool. */
    index = bullet_system->bullet_count++;
    bullet = &bullet_system->bullets[index];
    init_bullet (bullet, bullet_system, x, y, .1, 0, 0, 0, 0, script, type);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_movement (SCM bullet_ref_smob, SCM s_speed, SCM s_direction,
                     SCM s_acceleration, SCM s_angular_velocity) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float speed = scm_to_double (s_speed);
    float direction = deg2rad (scm_to_double (s_direction));
    float acceleration = scm_to_double (s_acceleration);
    float angular_velocity = scm_to_double (s_angular_velocity);

    init_bullet_movement (bullet, speed, direction, acceleration, angular_velocity);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_script (SCM bullet_ref_smob, SCM s_dt, SCM script) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    int dt = scm_to_int (s_dt);

    bullet->script = script;
    bullet->script_time = bullet->life_count + dt;

    return SCM_UNSPECIFIED;
}

static SCM
kill_bullet (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    bullet->kill = true;
    

    return SCM_UNSPECIFIED;
}

static SCM
bullet_x (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    return scm_from_double (bullet->x);
}

static SCM
bullet_y (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    return scm_from_double (bullet->y);
}

static SCM
bullet_speed (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    return scm_from_double (mag (bullet->dx, bullet->dy));
}

static SCM
bullet_direction (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float direction = rad2deg (atan2 (bullet->dy, bullet->dx));

    return scm_from_double (direction);
}

static SCM
bullet_acceleration (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    return scm_from_double (mag (bullet->ddx, bullet->ddy));
}

static SCM
bullet_angular_velocity (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float vx = 1;
    float vy = 0;
    float angular_velocity;

    al_transform_coordinates (&bullet->angular_velocity, &vx, &vy);
    angular_velocity = rad2deg (atan2 (vy, vx));

    return scm_from_double (angular_velocity);
}

static SCM
bullet_life (SCM bullet_ref_smob) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);

    return scm_from_int (bullet->life);
}

static SCM
set_bullet_x (SCM bullet_ref_smob, SCM s_x) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float x = scm_to_double (s_x);

    bullet->x = x;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_y (SCM bullet_ref_smob, SCM s_y) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float y = scm_to_double (s_y);

    bullet->y = y;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_position (SCM bullet_ref_smob, SCM s_x, SCM s_y) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    bullet->x = x;
    bullet->y = y;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_speed (SCM bullet_ref_smob, SCM s_speed) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float speed = scm_to_double (s_speed);
    float theta = atan2 (bullet->dy, bullet->dx);

    bullet->dx = cos (theta) * speed;
    bullet->dy = sin (theta) * speed;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_direction (SCM bullet_ref_smob, SCM s_direction) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float direction = scm_to_double (s_direction);
    float theta = deg2rad (direction);
    float speed = mag (bullet->dx, bullet->dy);
    float acceleration = mag (bullet->ddx, bullet->ddy);

    bullet->dx = cos (theta) * speed;
    bullet->dy = sin (theta) * speed;
    bullet->ddx = cos (theta) * acceleration;
    bullet->ddy = sin (theta) * acceleration;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_acceleration (SCM bullet_ref_smob, SCM s_acceleration) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float acceleration = scm_to_double (s_acceleration);
    float theta = atan2 (bullet->ddy, bullet->ddx);

    bullet->ddx = cos (theta) * acceleration;
    bullet->ddy = sin (theta) * acceleration;

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_angular_velocity (SCM bullet_ref_smob, SCM s_angular_velocity) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    float angular_velocity = scm_to_double (s_angular_velocity);
    float theta = deg2rad (angular_velocity);

    al_build_transform (&bullet->angular_velocity, 0, 0, 1, 1, theta);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_life (SCM bullet_ref_smob, SCM s_life) {
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    Bullet *bullet = bullet_from_id (bullet_ref->system, bullet_ref->id);
    int life = scm_to_int (s_life);

    bullet->life = life;

    return SCM_UNSPECIFIED;
}

void
init_bullet_system_type (void) {
    /* BulletSystem bindings */
    bullet_system_tag = scm_make_smob_type ("<bullet-system>", sizeof (BulletSystem));
    scm_set_smob_mark (bullet_system_tag, mark_bullet_system);
    scm_set_smob_free (bullet_system_tag, free_bullet_system);
    scm_set_smob_print (bullet_system_tag, print_bullet_system);

    scm_c_define_gsubr ("make-bullet-system", 2, 0, 0, make_bullet_system);
    scm_c_define_gsubr ("clear-bullet-system", 1, 0, 0, clear_bullet_system);
    scm_c_define_gsubr ("draw-bullet-system", 1, 0, 0, draw_bullet_system);
    scm_c_define_gsubr ("draw-bullet-system-hitboxes", 1, 0, 0,
                        draw_bullet_system_hitboxes);
    scm_c_define_gsubr ("update-bullet-system", 1, 0, 0, update_bullet_system);
    scm_c_define_gsubr ("set-bullet-system-sprite-sheet", 2, 0, 0,
                        set_bullet_system_sprite_sheet);
    scm_c_define_gsubr ("bullet-system-sprite-sheet", 1, 0, 0,
                        get_bullet_system_sprite_sheet);
    scm_c_define_gsubr ("bullet-system-collide-rect", 3, 0, 0,
                        bullet_system_collide_rect);
    scm_c_define_gsubr ("bullet-system-bounds", 1, 0, 0, bullet_system_bounds);
    scm_c_define_gsubr ("set-bullet-system-bounds", 5, 0, 0, set_bullet_system_bounds);
    scm_c_define_gsubr ("%emit-bullet", 9, 0, 0, emit_bullet);
    scm_c_define_gsubr ("%emit-simple-bullet", 6, 0, 0, emit_simple_bullet);
    scm_c_define_gsubr ("%emit-script-bullet", 5, 0, 0, emit_script_bullet);

    /* BulletRef bindings */
    bullet_ref_tag = scm_make_smob_type ("<bullet-ref>", sizeof (BulletRef));
    scm_set_smob_mark (bullet_ref_tag, 0);
    scm_set_smob_free (bullet_ref_tag, free_bullet_ref);
    scm_set_smob_print (bullet_ref_tag, print_bullet_ref);
    
    scm_c_define_gsubr ("set-bullet-movement", 5, 0, 0, set_bullet_movement);
    scm_c_define_gsubr ("kill-bullet", 1, 0, 0, kill_bullet);
    scm_c_define_gsubr ("bullet-x", 1, 0, 0, bullet_x);
    scm_c_define_gsubr ("bullet-y", 1, 0, 0, bullet_y);
    scm_c_define_gsubr ("bullet-speed", 1, 0, 0, bullet_speed);
    scm_c_define_gsubr ("bullet-direction", 1, 0, 0, bullet_direction);
    scm_c_define_gsubr ("bullet-acceleration", 1, 0, 0, bullet_acceleration);
    scm_c_define_gsubr ("bullet-angular-velocity", 1, 0, 0, bullet_angular_velocity);
    scm_c_define_gsubr ("bullet-life", 1, 0, 0, bullet_life);
    scm_c_define_gsubr ("set-bullet-script", 3, 0, 0, set_bullet_script);
    scm_c_define_gsubr ("set-bullet-x", 2, 0, 0, set_bullet_x);
    scm_c_define_gsubr ("set-bullet-y", 2, 0, 0, set_bullet_y);
    scm_c_define_gsubr ("set-bullet-position", 3, 0, 0, set_bullet_position);
    scm_c_define_gsubr ("set-bullet-speed", 2, 0, 0, set_bullet_speed);
    scm_c_define_gsubr ("set-bullet-direction", 2, 0, 0, set_bullet_direction);
    scm_c_define_gsubr ("set-bullet-acceleration", 2, 0, 0, set_bullet_acceleration);
    scm_c_define_gsubr ("set-bullet-angular-velocity", 2, 0, 0,
                        set_bullet_angular_velocity);
    scm_c_define_gsubr ("set-bullet-life", 2, 0, 0, set_bullet_life);

    /* BulletType bindings */
    bullet_type_tag = scm_make_smob_type ("<bullet-type>", sizeof (BulletType));
    scm_set_smob_mark (bullet_type_tag, 0);
    scm_set_smob_free (bullet_type_tag, free_bullet_type);
    scm_set_smob_print (bullet_type_tag, print_bullet_type);
    
    scm_c_define_gsubr ("make-bullet-type", 4, 0, 0, make_bullet_type);

    /* Exports */
    scm_c_export ("make-bullet-type", NULL);
    scm_c_export ("make-bullet-system", NULL);
    scm_c_export ("clear-bullet-system", NULL);
    scm_c_export ("draw-bullet-system", NULL);
    scm_c_export ("draw-bullet-system-hitboxes", NULL);
    scm_c_export ("update-bullet-system", NULL);
    scm_c_export ("set-bullet-system-sprite-sheet", NULL);
    scm_c_export ("bullet-system-sprite-sheet", NULL);
    scm_c_export ("bullet-system-collide-rect", NULL);
    scm_c_export ("bullet-system-bounds", NULL);
    scm_c_export ("set-bullet-system-bounds", NULL);
    scm_c_export ("%emit-bullet", NULL);
    scm_c_export ("%emit-simple-bullet", NULL);
    scm_c_export ("%emit-script-bullet", NULL);
    scm_c_export ("set-bullet-movement", NULL);
    scm_c_export ("kill-bullet", NULL);
    scm_c_export ("bullet-x", NULL);
    scm_c_export ("bullet-y", NULL);
    scm_c_export ("bullet-speed", NULL);
    scm_c_export ("bullet-direction", NULL);
    scm_c_export ("bullet-acceleration", NULL);
    scm_c_export ("bullet-angular-velocity", NULL);
    scm_c_export ("bullet-life", NULL);
    scm_c_export ("set-bullet-script", NULL);
    scm_c_export ("set-bullet-x", NULL);
    scm_c_export ("set-bullet-y", NULL);
    scm_c_export ("set-bullet-position", NULL);
    scm_c_export ("set-bullet-speed", NULL);
    scm_c_export ("set-bullet-direction", NULL);
    scm_c_export ("set-bullet-acceleration", NULL);
    scm_c_export ("set-bullet-angular-velocity", NULL);
    scm_c_export ("set-bullet-life", NULL);
}
