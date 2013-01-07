#include "bullet_system.h"

static scm_t_bits bullet_system_tag;
static scm_t_bits bullet_ref_tag;

SCM_KEYWORD (keyword_acceleration, "acceleration");
SCM_KEYWORD (keyword_angular_velocity, "angular-velocity");
SCM_KEYWORD (keyword_life, "life");
SCM_KEYWORD (keyword_color, "color");
SCM_KEYWORD (keyword_scale, "scale");

static GmkBulletSystem *
check_bullet_system (SCM bullet_system)
{
    scm_assert_smob_type (bullet_system_tag, bullet_system);

    return (GmkBulletSystem *) SCM_SMOB_DATA (bullet_system);
}

static GmkBulletRef *
check_bullet_ref (SCM bullet_ref)
{
    scm_assert_smob_type (bullet_ref_tag, bullet_ref);

    return (GmkBulletRef *) SCM_SMOB_DATA (bullet_ref);
}

static SCM
make_bullet_ref (GmkBulletSystem *bullet_system, int id)
{
    SCM smob;
    GmkBulletRef *bullet_ref;

    /* Error handling for out of range bullet ids */
    if (id < 0 || id >= bullet_system->max_bullets) {
        return SCM_BOOL_F;
    }

    bullet_ref = (GmkBulletRef *) scm_gc_malloc (sizeof (GmkBulletRef), "bullet ref");
    bullet_ref->system = bullet_system;
    bullet_ref->id = id;
    SCM_NEWSMOB (smob, bullet_ref_tag, bullet_ref);

    return smob;
}

static GmkBullet *
bullet_from_index (GmkBulletSystem *bullet_system, int index)
{
    return bullet_system->bullets + index;
}

static int
bullet_index_from_id (GmkBulletSystem *bullet_system, int id)
{
    return bullet_system->bullet_ids[id];
}

static GmkBullet *
bullet_from_id (GmkBulletSystem *bullet_system, int id)
{
    return bullet_from_index (bullet_system,
                              bullet_index_from_id (bullet_system, id));
}

static void
init_bullets (GmkBulletSystem *bullet_system)
{
    for (int i = 0; i < bullet_system->max_bullets; ++i) {
        GmkBullet *bullet = bullet_from_index (bullet_system, i);

        bullet->id = i;
        bullet->ref = make_bullet_ref (bullet_system, i);
        bullet->active = false;
        bullet->script = SCM_BOOL_F;
        bullet_system->bullet_ids[i] = i;
    }
}

static int
bullets_size (GmkBulletSystem *bullet_system)
{
    return sizeof (GmkBullet) * bullet_system->max_bullets;
}

static int
bullet_ids_size (GmkBulletSystem *bullet_system)
{
    return sizeof (int) * bullet_system->max_bullets;
}

static bool
bullet_out_of_bounds(GmkBulletSystem *bullet_system, GmkBullet *bullet)
{
    return !gmk_rect_collide_point (bullet_system->bounds, bullet->pos);
}

static void
free_bullet (GmkBulletSystem *bullet_system, int index)
{
    GmkBullet *bullet = bullet_system->bullets + index;
    GmkBullet temp;
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

static bool
bullet_has_script (GmkBullet *bullet)
{
    return scm_is_true (scm_procedure_p (bullet->script));
}

static void
update_bullet_script (GmkBullet *bullet)
{
    /* Call script if there is a script to run and it's time to run it. */
    if (bullet->life_count == bullet->script_time && bullet_has_script (bullet)) {
        scm_call_1 (bullet->script, bullet->ref);
    }
}

static void
update_bullet(GmkBullet *bullet)
{
    update_bullet_script (bullet);
    al_transform_coordinates (&bullet->angular_velocity,
                              &bullet->vel.x, &bullet->vel.y);
    al_transform_coordinates (&bullet->angular_velocity,
                              &bullet->acc.x, &bullet->acc.y);
    bullet->pos = gmk_vector2_add (bullet->pos, bullet->vel);
    bullet->vel = gmk_vector2_add (bullet->vel, bullet->acc);
    bullet->life_count++;
}

static bool
bullet_has_life_remaining (GmkBullet *bullet)
{
    /* Life == 0 means unlimited lifetime. */
    if (bullet->life == 0) {
        return true;
    }

    return bullet->life_count < bullet->life;
}

static bool
bullet_dead (GmkBulletSystem *bullet_system, GmkBullet *bullet)
{
    return (bullet->kill ||
            bullet_out_of_bounds (bullet_system, bullet) ||
            !bullet_has_life_remaining (bullet));
}

static void
set_bullet_blend_mode (GmkBullet *bullet, GmkBlendMode prev_blend_mode)
{
    if (bullet->blend_mode != prev_blend_mode) {
        /* It is not possible to hold bitmap drawing and
         * change the blending mode.
         */
        al_hold_bitmap_drawing (false);
        gmk_set_blend_mode (bullet->blend_mode);
        al_hold_bitmap_drawing (true);
    }
}

static float
bullet_sprite_angle (GmkBullet *bullet)
{
    if (bullet->directional) {
        return gmk_vector2_angle (bullet->vel);
    }

    return 0;
}

static void
draw_bullet (GmkBullet *bullet, int cx, int cy, GmkBlendMode prev_blend_mode)
{
    float angle = bullet_sprite_angle (bullet);
    ALLEGRO_COLOR color = gmk_color_mult_alpha (bullet->color);

    set_bullet_blend_mode (bullet, prev_blend_mode);
    al_draw_tinted_scaled_rotated_bitmap (bullet->image, color, cx, cy,
                                          bullet->pos.x, bullet->pos.y,
                                          bullet->scale.x, bullet->scale.y,
                                          angle, 0);
}

static bool
bullet_collision_check (GmkBullet *bullet, GmkRect rect, SCM callback)
{
    GmkRect hitbox = gmk_rect_move (bullet->hitbox, bullet->pos);

    if (gmk_rect_collide_rect (hitbox, rect)) {
        if (scm_procedure_p (callback)) {
            /*
             * The callback can return true if the bullet should be
             * removed from the system.
             */
            return scm_is_true (scm_call_1 (callback, bullet->ref));
        }
    }

    return false;
}

static GmkBullet *
new_bullet (GmkBulletSystem *bullet_system) {
    int index;

    if (bullet_system->bullet_count >= bullet_system->max_bullets) {
        return NULL;
    }

    /* Get new bullet from pool. */
    index = bullet_system->bullet_count++;

    return bullet_system->bullets + index;
}

static void
init_bullet_movement (GmkBullet *bullet, float speed, float direction,
                      float acceleration, float angular_velocity)
{
    float theta = gmk_deg_to_rad (direction);

    bullet->vel = gmk_vector2_from_polar (speed, theta);
    bullet->acc = gmk_vector2_from_polar (acceleration, theta);
    al_build_transform (&bullet->angular_velocity, 0, 0, 1, 1,
                        gmk_deg_to_rad (angular_velocity));
}

static void
init_bullet_type (GmkBullet *bullet, GmkBulletSystem *system, GmkBulletType *type)
{
    GmkSpriteSheet *sprite_sheet = gmk_scm_to_sprite_sheet (system->sprite_sheet);

    bullet->directional = type->directional;
    bullet->image = gmk_sprite_sheet_tile (sprite_sheet, type->image);
    bullet->blend_mode = type->blend_mode;
    bullet->hitbox = gmk_rect_scale (type->hitbox, bullet->scale);
}

static void
init_bullet (GmkBullet *bullet, GmkBulletSystem *bullet_system, GmkVector2 pos,
             float speed, float direction, float acceleration, float angular_velocity,
             int life, SCM script, ALLEGRO_COLOR color, GmkVector2 scale,
             GmkBulletType *type)
{
    bullet->active = true;
    bullet->kill = false;
    bullet->life = life;
    bullet->script_time = 0;
    bullet->life_count = 0;
    bullet->pos = pos;
    bullet->scale = scale;
    bullet->color = color;
    bullet->script = script;
    init_bullet_movement (bullet, speed, direction, acceleration, angular_velocity);
    init_bullet_type (bullet, bullet_system, type);
}

SCM_DEFINE (gmk_s_make_bullet_system, "make-bullet-system", 2, 0, 0,
            (SCM max_bullets, SCM sprite_sheet),
            "Make a new bullet system and allocate memory for @var{max_bullets} "
            "bullets.")
{
    SCM smob;
    GmkBulletSystem *bullet_system;

    bullet_system = (GmkBulletSystem *) scm_gc_malloc (sizeof (GmkBulletSystem),
                                                       "bullet system");
    bullet_system->max_bullets = scm_to_int (max_bullets);
    bullet_system->bullet_count = 0;
    bullet_system->bullets = NULL;
    bullet_system->sprite_sheet = SCM_BOOL_F;

    SCM_NEWSMOB (smob, bullet_system_tag, bullet_system);

    bullet_system->bullets = (GmkBullet *) scm_gc_malloc (bullets_size (bullet_system),
                                                          "bullets");
    bullet_system->bullet_ids = (int *) scm_gc_malloc (bullet_ids_size (bullet_system),
                                                       "bullet ids");
    bullet_system->sprite_sheet = sprite_sheet;
    init_bullets (bullet_system);

    return smob;
}

SCM_DEFINE (gmk_s_clear_bullet_system, "clear-bullet-system", 1, 0, 0,
    (SCM bullet_system),
    "Remove all bullets from bullet system.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    for (int i = 0; i < system->bullet_count; ++i) {
        GmkBullet *bullet = bullet_from_index (system, i);

        bullet->active = false;
    }

    system->bullet_count = 0;
    scm_remember_upto_here_1 (bullet_system);

return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_max_bullets, "max-bullets", 1, 0, 0,
            (SCM bullet_system),
            "Return max bullets in bullet system.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    return scm_from_int (system->max_bullets);
}

SCM_DEFINE (gmk_s_set_bullet_system_bounds, "set-bullet-system-bounds", 2, 0, 0,
            (SCM bullet_system, SCM rect),
            "Set the bounding box. Bullets outside of the bounding box are killed.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    system->bounds = gmk_scm_to_rect (rect);
    scm_remember_upto_here_1 (bullet_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_bullet_system_bounds, "bullet-system-bounds", 1, 0, 0,
            (SCM bullet_system),
            "Return bullet system bounding box.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    return gmk_scm_from_rect (system->bounds);
}

SCM_DEFINE (gmk_s_bullet_system_count, "bullet-system-count", 1, 0, 0,
            (SCM bullet_system),
            "Return number of active bullets.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    return scm_from_int (system->bullet_count);
}


SCM_DEFINE (gmk_s_update_bullet_system, "update-bullet-system", 1, 0, 0,
            (SCM bullet_system),
            "Update bullet system.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    for (int i = 0; i < system->bullet_count; ++i) {
        GmkBullet *bullet = bullet_from_index (system, i);

        if (bullet->active) {
            update_bullet (bullet);

            /* Remove bullets that are outside of the game field or have no lifetime
             * remaining. Only unscripted bullets are removed. */
            if (bullet_dead (system, bullet)) {
                free_bullet (system, i);
                --i;
            }
        }
    }

    scm_remember_upto_here_1 (bullet_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_draw_bullet_system, "draw-bullet-system", 1, 0, 0,
            (SCM bullet_system),
            "Draw bullet system.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);
    GmkSpriteSheet *sprite_sheet = gmk_scm_to_sprite_sheet (system->sprite_sheet);
    GmkBlendMode prev_blend_mode = -1;
    ALLEGRO_STATE state;

    al_store_state (&state, ALLEGRO_STATE_BLENDER);
    al_hold_bitmap_drawing (true);

    for (int i = 0; i < system->bullet_count; ++i) {
        GmkBullet *bullet = system->bullets + i;

        draw_bullet (bullet, sprite_sheet->tile_width / 2,
                     sprite_sheet->tile_height / 2,
                     prev_blend_mode);
        prev_blend_mode = bullet->blend_mode;
    }

    al_hold_bitmap_drawing (false);
    al_restore_state (&state);
    scm_remember_upto_here_1 (bullet_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_draw_bullet_system_hitboxes, "draw-bullet-system-hitboxes", 1, 0, 0,
            (SCM bullet_system),
            "Draw all bullet hitboxes. Useful for debugging.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    for (int i = 0; i < system->bullet_count; ++i) {
        GmkBullet *bullet = bullet_from_index (system, i);

        if (bullet->active && bullet->image) {
            GmkRect hitbox = gmk_rect_move (bullet->hitbox, bullet->pos);
            al_draw_rectangle (hitbox.x, hitbox.y,
                               hitbox.x + hitbox.width, hitbox.y + hitbox.height,
                               al_map_rgba_f (1, 0, 1, 1), 2);
        }
    }

    scm_remember_upto_here_1 (bullet_system);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_bullet_system_collide_rect, "bullet-system-collide-rect", 3, 0, 0,
            (SCM bullet_system, SCM rect, SCM callback),
            "Test every bullet in the system for collision with @var{rect}. "
            "If a collision occurs, @var{callback} will be called. "
            "@var{callback} accepts one argument: a bullet. "
            "Return @code{#t} from the callback to kill the bullet.")
{
    GmkBulletSystem *system = check_bullet_system(bullet_system);
    GmkRect r = gmk_scm_to_rect (rect);

    for (int i = 0; i < system->bullet_count; ++i) {
        GmkBullet *bullet = bullet_from_index (system, i);

        if (bullet->active && bullet_collision_check (bullet, r, callback)) {
            bullet->kill = true;
        }
    }

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_bullet_system_sprite_sheet, "bullet-system-sprite-sheet", 1, 0, 0,
            (SCM bullet_system),
            "Return sprite sheet.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    return system->sprite_sheet;
}

SCM_DEFINE (gmk_s_set_bullet_system_sprite_sheet, "set-bullet-system-sprite-sheet",
            2, 0, 0,
            (SCM bullet_system, SCM sprite_sheet_smob),
            "Set sprite sheet.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);

    system->sprite_sheet = sprite_sheet_smob;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_emit_bullet, "%emit-bullet", 5, 0, 1,
            (SCM bullet_system, SCM pos, SCM speed, SCM direction,
             SCM type, SCM kw_args),
            "Emit bullet. Additional parameters are available as keyword "
            "arguments: acceleration, angular-velocity, scale, life, color.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);
    GmkBullet *bullet = new_bullet (system);
    /* Get keyword arguments, falling back on sane defaults. */
    SCM default_color = gmk_scm_from_color (al_map_rgba_f (1, 1, 1, 1));
    SCM default_scale = gmk_scm_from_vector2 (gmk_vector2_new (1, 1));
    float acceleration = scm_to_double (scm_get_keyword (keyword_acceleration,
                                                         kw_args,
                                                         scm_from_double (0)));
    float angular_velocity = scm_to_double (scm_get_keyword (keyword_angular_velocity,
                                                             kw_args,
                                                             scm_from_double (0)));
    GmkVector2 scale = gmk_scm_to_vector2 (scm_get_keyword (keyword_scale,
                                                            kw_args,
                                                            default_scale));
    int life = scm_to_int (scm_get_keyword (keyword_life,
                                            kw_args,
                                            scm_from_int (0)));
    ALLEGRO_COLOR color = gmk_scm_to_color (scm_get_keyword (keyword_color,
                                                             kw_args,
                                                             default_color));

    if (bullet) {
        init_bullet (bullet, system, gmk_scm_to_vector2 (pos), scm_to_double (speed),
                     scm_to_double (direction), acceleration, angular_velocity,
                     life, SCM_BOOL_F, color, scale, gmk_check_bullet_type (type));
    }

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_emit_script_bullet, "%emit-script-bullet", 4, 0, 0,
            (SCM bullet_system, SCM pos, SCM type, SCM script),
            "Emit a scripted bullet.")
{
    GmkBulletSystem *system = check_bullet_system (bullet_system);
    GmkBullet *bullet = new_bullet (system);

    if (bullet) {
        init_bullet (bullet, system, gmk_scm_to_vector2 (pos), .1, 0, 0,
                     0, 0, script, al_map_rgba_f (1, 1, 1, 1),
                     gmk_vector2_new (1, 1), gmk_check_bullet_type (type));
    }

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_movement, "set-bullet-movement", 5, 0, 0,
            (SCM bullet_ref, SCM speed, SCM direction,
             SCM acceleration, SCM angular_velocity),
            "Set bullet movement properties.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    init_bullet_movement (bullet, scm_to_double (speed),
                          scm_to_double (direction),
                          scm_to_double (acceleration),
                          scm_to_double (angular_velocity));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_script, "set-bullet-script", 3, 0, 0,
            (SCM bullet_ref, SCM dt, SCM script),
            "Set @var{script} to be called after @var{dt} frames have passed.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->script = script;
    bullet->script_time = bullet->life_count + scm_to_int (dt);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_type, "%set-bullet-type", 2, 0, 0,
            (SCM bullet_ref, SCM bullet_type),
            "Change bullet type.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBulletType *type = gmk_check_bullet_type (bullet_type);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    init_bullet_type (bullet, ref->system, type);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_kill_bullet, "kill-bullet", 1, 0, 0,
            (SCM bullet_ref),
            "Kill bullet.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->kill = true;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_bullet_position, "bullet-position", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet position.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return gmk_scm_from_vector2 (bullet->pos);
}

SCM_DEFINE (gmk_s_bullet_speed, "bullet-speed", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet speed.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return scm_from_double (gmk_vector2_mag (bullet->vel));
}

SCM_DEFINE (gmk_s_bullet_direction, "bullet-direction", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet direction in degrees.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);
    float direction = gmk_rad_to_deg (gmk_vector2_angle (bullet->vel));

    return scm_from_double (direction);
}

SCM_DEFINE (gmk_s_bullet_acceleration, "bullet-acceleration", 1, 0, 0,
    (SCM bullet_ref),
    "Return bullet acceleration.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return scm_from_double (gmk_vector2_mag (bullet->acc));
}

SCM_DEFINE (gmk_s_bullet_angular_velocity, "bullet-angular-velocity", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet angular velocity")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);
    float vx = 1;
    float vy = 0;
    float angular_velocity;

    al_transform_coordinates (&bullet->angular_velocity, &vx, &vy);
    angular_velocity = gmk_rad_to_deg (atan2 (vy, vx));

    return scm_from_double (angular_velocity);
}

SCM_DEFINE (gmk_s_bullet_life, "bullet-life", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet lifespan.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return scm_from_int (bullet->life);
}

SCM_DEFINE (gmk_s_bullet_color, "bullet-color", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet color")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return gmk_scm_from_color (bullet->color);
}

SCM_DEFINE (gmk_s_bullet_scale, "bullet-scale", 1, 0, 0,
            (SCM bullet_ref),
            "Return bullet scale vector.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    return gmk_scm_from_vector2 (bullet->scale);
}

SCM_DEFINE (gmk_s_set_bullet_position, "set-bullet-position", 2, 0, 0,
            (SCM bullet_ref, SCM pos),
            "Set bullet position vector.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->pos = gmk_scm_to_vector2 (pos);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_speed, "set-bullet-speed", 2, 0, 0,
            (SCM bullet_ref, SCM speed),
            "Set bullet speed.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->vel = gmk_vector2_scale (gmk_vector2_norm (bullet->vel),
                                     scm_to_double (speed));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_direction, "set-bullet-direction", 2, 0, 0,
            (SCM bullet_ref, SCM direction),
            "Set bullet direction.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);
    float theta = gmk_deg_to_rad (scm_to_double (direction));
    float speed = gmk_vector2_mag (bullet->vel);
    float acceleration = gmk_vector2_mag (bullet->acc);

    bullet->vel = gmk_vector2_from_polar (speed, theta);
    bullet->acc = gmk_vector2_from_polar (acceleration, theta);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_acceleration, "set-bullet-acceleration", 2, 0, 0,
            (SCM bullet_ref, SCM acceleration),
            "Set bullet acceleration.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->acc = gmk_vector2_scale (gmk_vector2_norm (bullet->acc),
                                     scm_to_double (acceleration));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_angular_velocity, "set-bullet-angular-velocity", 2, 0, 0,
            (SCM bullet_ref, SCM angular_velocity),
            "Set bullet angular velocity.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);
    float theta = gmk_deg_to_rad (scm_to_double (angular_velocity));

    al_build_transform (&bullet->angular_velocity, 0, 0, 1, 1, theta);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_life, "set-bullet-life", 2, 0, 0,
            (SCM bullet_ref, SCM life),
            "Set bullet lifespan.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->life = scm_to_int (life);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_color, "set-bullet-color", 2, 0, 0,
            (SCM bullet_ref, SCM color),
            "Set bullet color")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->color = gmk_scm_to_color (color);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_set_bullet_scale, "set-bullet-scale", 2, 0, 0,
            (SCM bullet_ref, SCM scale),
            "Set bullet scale vector.")
{
    GmkBulletRef *ref = check_bullet_ref (bullet_ref);
    GmkBullet *bullet = bullet_from_id (ref->system, ref->id);

    bullet->scale = gmk_scm_to_vector2 (scale);

    return SCM_UNSPECIFIED;
}

static SCM
mark_bullet_system (SCM bullet_system)
{
    GmkBulletSystem *system = (GmkBulletSystem *) SCM_SMOB_DATA (bullet_system);

    return system->sprite_sheet;
}

static size_t
free_bullet_system (SCM bullet_system)
{
    GmkBulletSystem *system = (GmkBulletSystem *) SCM_SMOB_DATA (bullet_system);

    scm_gc_free (system->bullets, bullets_size (system), "bullets");
    scm_gc_free (system->bullet_ids, bullet_ids_size (system),
                 "bullet ids");
    scm_gc_free (system, sizeof (GmkBulletSystem), "bullet system");

    return 0;
}

static int
print_bullet_system (SCM bullet_system, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<GmkBulletSystem max-bullets: ", port);
    scm_display (gmk_s_max_bullets (bullet_system), port);
    scm_puts (">", port);

    return 1;
}

static size_t
free_bullet_ref (SCM bullet_ref)
{
    GmkBulletRef *ref = (GmkBulletRef *) SCM_SMOB_DATA (bullet_ref);

    scm_gc_free (ref, sizeof (GmkBulletRef), "bullet ref");

    return 0;
}

static int
print_bullet_ref (SCM bullet_ref, SCM port, scm_print_state *pstate) {
    GmkBulletRef *ref = (GmkBulletRef *) SCM_SMOB_DATA (bullet_ref);

    scm_puts ("#<bullet-ref ", port);
    scm_display (scm_from_double (ref->id), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_bullet_system (void) {
    bullet_system_tag = scm_make_smob_type ("bullet-system", sizeof (GmkBulletSystem));
    scm_set_smob_mark (bullet_system_tag, mark_bullet_system);
    scm_set_smob_free (bullet_system_tag, free_bullet_system);
    scm_set_smob_print (bullet_system_tag, print_bullet_system);
    bullet_ref_tag = scm_make_smob_type ("bullet-ref", sizeof (GmkBulletRef));
    scm_set_smob_mark (bullet_ref_tag, 0);
    scm_set_smob_free (bullet_ref_tag, free_bullet_ref);
    scm_set_smob_print (bullet_ref_tag, print_bullet_ref);

#include "bullet_system.x"

    scm_c_export (s_gmk_s_make_bullet_system,
                  s_gmk_s_clear_bullet_system,
                  s_gmk_s_max_bullets,
                  s_gmk_s_draw_bullet_system,
                  s_gmk_s_draw_bullet_system_hitboxes,
                  s_gmk_s_update_bullet_system,
                  s_gmk_s_set_bullet_system_sprite_sheet,
                  s_gmk_s_bullet_system_sprite_sheet,
                  s_gmk_s_bullet_system_collide_rect,
                  s_gmk_s_bullet_system_bounds,
                  s_gmk_s_bullet_system_count,
                  s_gmk_s_set_bullet_system_bounds,
                  s_gmk_s_emit_bullet,
                  s_gmk_s_emit_script_bullet,
                  s_gmk_s_set_bullet_movement,
                  s_gmk_s_kill_bullet,
                  s_gmk_s_bullet_position,
                  s_gmk_s_bullet_speed,
                  s_gmk_s_bullet_direction,
                  s_gmk_s_bullet_acceleration,
                  s_gmk_s_bullet_angular_velocity,
                  s_gmk_s_bullet_life,
                  s_gmk_s_bullet_color,
                  s_gmk_s_bullet_scale,
                  s_gmk_s_set_bullet_script,
                  s_gmk_s_set_bullet_type,
                  s_gmk_s_set_bullet_position,
                  s_gmk_s_set_bullet_speed,
                  s_gmk_s_set_bullet_direction,
                  s_gmk_s_set_bullet_acceleration,
                  s_gmk_s_set_bullet_angular_velocity,
                  s_gmk_s_set_bullet_life,
                  s_gmk_s_set_bullet_color,
                  s_gmk_s_set_bullet_scale,
                  NULL);
}
