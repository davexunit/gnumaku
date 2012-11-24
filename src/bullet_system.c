#include "bullet_system.h"

static scm_t_bits bullet_system_tag;
static scm_t_bits bullet_ref_tag;

static SCM
nnnmake_bullet_ref (SCM bullet_system_smob);

static BulletSystem*
check_bullet_system (SCM bullet_system_smob)
{
    scm_assert_smob_type (bullet_system_tag, bullet_system_smob);

    return (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);
}

static BulletRef*
check_bullet_ref (SCM bullet_ref_smob)
{
    scm_assert_smob_type (bullet_ref_tag, bullet_ref_smob);

    return (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);
}

static void
init_bullet (Bullet *bullet)
{
    bullet->speed = 0;
    bullet->direction = 0;
    bullet->acceleration = 0;
    bullet->angular_velocity = 0;
    bullet->x = 0;
    bullet->y = 0;
    bullet->alive = false;
    bullet->referenced = false;
    bullet->killable = true;
    bullet->image = NULL;
    bullet->color = al_map_rgba_f (1, 1, 1, 1);
    init_rect (&bullet->hitbox, 0, 0, 0, 0);
}

static SCM
make_bullet_system (SCM s_max_bullets)
{
    SCM smob;
    BulletSystem *bullet_system;
    int max_bullets = scm_to_int (s_max_bullets);

    /* Step 1: Allocate the memory block.
     */
    bullet_system = (BulletSystem *) scm_gc_malloc (sizeof (BulletSystem), "bullet_system");

    /* Step 2: Initialize it with straight code.
     */
    bullet_system->max_bullets = max_bullets;
    bullet_system->bullets = NULL;
    bullet_system->sprite_sheet = SCM_BOOL_F;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, bullet_system_tag, bullet_system);

    /* Step 4: Finish the initialization.
     */
    bullet_system->bullets = (Bullet *) scm_gc_malloc (sizeof (Bullet) * max_bullets, "bullets");

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        init_bullet (bullet_system->bullets + i);
    }

    return smob;
}

static SCM
mark_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    return bullet_system->sprite_sheet;
}

static size_t
free_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    scm_gc_free (bullet_system->bullets, sizeof (Bullet) * bullet_system->max_bullets, "bullets");
    scm_gc_free (bullet_system, sizeof (BulletSystem), "bullet_system");

    return 0;
}

static int
print_bullet_system (SCM bullet_system_smob, SCM port, scm_print_state *pstate)
{
    BulletSystem *bullet_system = (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);

    scm_puts ("#<BulletSystem ", port);
    scm_display (scm_from_int(bullet_system->max_bullets), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

static Bullet*
get_bullet_at_index (BulletSystem *bullet_system, int i)
{
    return bullet_system->bullets + i;
}

static int
free_bullet_index (BulletSystem *bullet_system)
{
    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        if (!get_bullet_at_index (bullet_system, i)->alive)
            return i;
    }

    return -1;
}

static Bullet*
get_free_bullet (BulletSystem *bullet_system)
{
    return get_bullet_at_index (bullet_system, free_bullet_index (bullet_system));
}

static SCM
clear_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        Bullet *bullet = bullet_system->bullets + i;
        init_bullet(bullet);

    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static void
update_bullet(Bullet *bullet, float dt) {
    float theta = deg2rad(bullet->direction);
    float dx = bullet->speed * cos(theta) * dt;
    float dy = bullet->speed * sin(theta) * dt;
    bullet->x += dx;
    bullet->y += dy;
    bullet->speed += bullet->acceleration * dt;
    bullet->direction += bullet->angular_velocity * dt;
}

static void
remove_out_of_bounds_bullet (Bullet *bullet)
{
    // hard-coded values for now
    static float x = -64;
    static float y = -64;
    static float width = 480 + 128;
    static float height = 560 + 128;

    if ((bullet->x < x ||
         bullet->x > x + width ||
         bullet->y < y ||
         bullet->y > y + height) &&
        (bullet->killable || !bullet->referenced))
    {
        bullet->alive = false;
    }
}

static SCM
update_bullet_system (SCM bullet_system_smob, SCM s_dt)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    float dt = scm_to_double (s_dt);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->alive)
        {
            update_bullet (bullet, dt);
            remove_out_of_bounds_bullet (bullet);
        }
    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
draw_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet (bullet_system->sprite_sheet);

    al_hold_bitmap_drawing (true);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->alive && bullet->image)
        {
            al_draw_rotated_bitmap (bullet->image, sprite_sheet->tile_width / 2, sprite_sheet->tile_height / 2, bullet->x, bullet->y, deg2rad(bullet->direction), 0);
        }
    }

    al_hold_bitmap_drawing (false);
    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
draw_bullet_system_hitboxes (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->alive && bullet->image)
        {
            Rect hitbox = rect_move(&bullet->hitbox, bullet->x, bullet->y);
            al_draw_rectangle (hitbox.x, hitbox.y, hitbox.x + hitbox.width, hitbox.y + hitbox.height,
                               al_map_rgba_f (1, 0, 1, 1), 2);
        }
    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static bool
bullet_collision_check (Bullet *bullet, Rect *rect, SCM callback)
{
    Rect hitbox = rect_move(&bullet->hitbox, bullet->x, bullet->y);

    if (rect_collide_rect (&hitbox, rect))
    {
        if (scm_procedure_p (callback))
        {
            /* The callback can return true if the bullet should be removed from the system */
            return scm_to_bool (scm_call_0 (callback));
        }
    }

    return false;
}

static SCM
bullet_system_collide_rect (SCM bullet_system_smob, SCM rect_smob, SCM callback)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    Rect *rect = check_rect (rect_smob);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
        Bullet *bullet = bullet_system->bullets + i;

        if (bullet->alive && bullet_collision_check (bullet, rect, callback))
        {
            bullet->alive = false;
        }
    }

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_system_sprite_sheet (SCM bullet_system_smob, SCM sprite_sheet_smob)
{
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);

    bullet_system->sprite_sheet = sprite_sheet_smob;
    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
make_bullet_ref (SCM bullet_system_smob)
{
    SCM smob;
    BulletRef *bullet_ref;
    BulletSystem *bullet_system = check_bullet_system (bullet_system_smob);
    Bullet *bullet = get_free_bullet (bullet_system);

    /* Step 1: Allocate the memory block.
     */
    bullet_ref = (BulletRef *) scm_gc_malloc (sizeof (BulletRef), "bullet_ref");

    /* Step 2: Initialize it with straight code.
     */
    bullet_ref->bullet_system = NULL;
    bullet_ref->bullet = NULL;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, bullet_ref_tag, bullet_ref);

    /* Step 4: Finish the initialization.
     */
    init_bullet(bullet);
    bullet->alive = true;
    bullet->referenced = true;
    bullet_ref->bullet_system = bullet_system;
    bullet_ref->bullet = bullet;

    return smob;
}

static size_t
free_bullet_ref (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);

    // Flip referenced flag so that the bullet system can kill this bullet.
    bullet_ref->bullet->referenced = false;

    scm_gc_free (bullet_ref, sizeof (BulletRef), "bullet_ref");

    return 0;
}

static int
print_bullet_ref (SCM bullet_ref_smob, SCM port, scm_print_state *pstate)
{
    BulletRef *bullet_ref = (BulletRef *) SCM_SMOB_DATA (bullet_ref_smob);
    Bullet *bullet = bullet_ref->bullet;

    scm_puts ("#<BulletRef ", port);
    scm_display (scm_from_double (bullet->x), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (bullet->y), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (bullet->speed), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (bullet->direction), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (bullet->acceleration), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (bullet->angular_velocity), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

static SCM
set_bullet_position (SCM bullet_ref_smob, SCM s_x, SCM s_y)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    bullet_ref->bullet->x = x;
    bullet_ref->bullet->y = y;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_speed (SCM bullet_ref_smob, SCM s_speed)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float speed = scm_to_double (s_speed);

    bullet_ref->bullet->speed = speed;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_acceleration (SCM bullet_ref_smob, SCM s_acceleration)
{
    BulletRef *bullet_ref = check_bullet_ref(bullet_ref_smob);
    float acceleration = scm_to_double(s_acceleration);

    bullet_ref->bullet->acceleration = acceleration;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_direction (SCM bullet_ref_smob, SCM s_direction)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float direction = scm_to_double (s_direction);

    bullet_ref->bullet->direction = direction;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
change_bullet_direction (SCM bullet_ref_smob, SCM s_direction)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float direction = scm_to_double (s_direction);

    bullet_ref->bullet->direction += direction;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_angular_velocity (SCM bullet_ref_smob, SCM s_angular_velocity)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float angular_velocity = scm_to_double (s_angular_velocity);

    bullet_ref->bullet->angular_velocity = angular_velocity;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_sprite (SCM bullet_ref_smob, SCM s_sprite_index)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet (bullet_ref->bullet_system->sprite_sheet);
    int sprite_index = scm_to_int (s_sprite_index);

    bullet_ref->bullet->image = sprite_sheet->tiles[sprite_index];

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_hitbox (SCM bullet_ref_smob, SCM s_x, SCM s_y, SCM s_width, SCM s_height)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float width = scm_to_double (s_width);
    float height = scm_to_double (s_height);

    bullet_ref->bullet->hitbox.x = x;
    bullet_ref->bullet->hitbox.y = y;
    bullet_ref->bullet->hitbox.width = width;
    bullet_ref->bullet->hitbox.height = height;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
kill_bullet (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    bullet_ref->bullet->alive = false;

    scm_remember_upto_here_1 (bullet_ref_smob);

    return SCM_UNSPECIFIED;
}

static SCM
bullet_x (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->x);
}

static SCM
bullet_y (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->y);
}

static SCM
bullet_speed (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->speed);
}

static SCM
bullet_direction (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->direction);
}

static SCM
bullet_acceleration (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref(bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->acceleration);
}

static SCM
bullet_angular_velocity (SCM bullet_ref_smob)
{
    BulletRef *bullet_ref = check_bullet_ref (bullet_ref_smob);

    scm_remember_upto_here_1 (bullet_ref_smob);

    return scm_from_double (bullet_ref->bullet->angular_velocity);
}

void
init_bullet_system_type (void)
{
    bullet_system_tag = scm_make_smob_type ("BulletSystem", sizeof (BulletSystem));
    scm_set_smob_mark (bullet_system_tag, mark_bullet_system);
    scm_set_smob_free (bullet_system_tag, free_bullet_system);
    scm_set_smob_print (bullet_system_tag, print_bullet_system);

    scm_c_define_gsubr ("make-bullet-system", 1, 0, 0, make_bullet_system);
    scm_c_define_gsubr ("clear-bullet-system!", 1, 0, 0, clear_bullet_system);
    scm_c_define_gsubr ("draw-bullet-system", 1, 0, 0, draw_bullet_system);
    scm_c_define_gsubr ("draw-bullet-system-hitboxes", 1, 0, 0, draw_bullet_system_hitboxes);
    scm_c_define_gsubr ("update-bullet-system!", 2, 0, 0, update_bullet_system);
    scm_c_define_gsubr ("set-bullet-system-sprite-sheet!", 2, 0, 0, set_bullet_system_sprite_sheet);
    scm_c_define_gsubr ("bullet-system-collide-rect", 3, 0, 0, bullet_system_collide_rect);

    scm_c_export ("make-bullet-system", NULL);
    scm_c_export ("clear-bullet-system!", NULL);
    scm_c_export ("draw-bullet-system", NULL);
    scm_c_export ("draw-bullet-system-hitboxes", NULL);
    scm_c_export ("update-bullet-system!", NULL);
    scm_c_export ("set-bullet-system-sprite-sheet!", NULL);
    scm_c_export ("bullet-system-collide-rect", NULL);

    bullet_ref_tag = scm_make_smob_type ("BulletRef", sizeof (BulletRef));
    scm_set_smob_mark (bullet_ref_tag, 0);
    scm_set_smob_free (bullet_ref_tag, free_bullet_ref);
    scm_set_smob_print (bullet_ref_tag, print_bullet_ref);

    scm_c_define_gsubr ("make-bullet", 1, 0, 0, make_bullet_ref);
    scm_c_define_gsubr ("kill-bullet", 1, 0, 0, kill_bullet);
    scm_c_define_gsubr ("bullet-x", 1, 0, 0, bullet_x);
    scm_c_define_gsubr ("bullet-y", 1, 0, 0, bullet_y);
    scm_c_define_gsubr ("bullet-y", 1, 0, 0, bullet_y);
    scm_c_define_gsubr ("bullet-speed", 1, 0, 0, bullet_speed);
    scm_c_define_gsubr ("bullet-direction", 1, 0, 0, bullet_direction);
    scm_c_define_gsubr ("bullet-acceleration", 1, 0, 0, bullet_acceleration);
    scm_c_define_gsubr ("bullet-angular-velocity", 1, 0, 0, bullet_angular_velocity);
    scm_c_define_gsubr ("set-bullet-position!", 3, 0, 0, set_bullet_position);
    scm_c_define_gsubr ("set-bullet-speed!", 2, 0, 0, set_bullet_speed);
    scm_c_define_gsubr ("set-bullet-direction!", 2, 0, 0, set_bullet_direction);
    scm_c_define_gsubr ("set-bullet-acceleration!", 2, 0, 0, set_bullet_acceleration);
    scm_c_define_gsubr ("set-bullet-angular-velocity!", 2, 0, 0, set_bullet_angular_velocity);
    scm_c_define_gsubr ("set-bullet-sprite!", 2, 0, 0, set_bullet_sprite);
    scm_c_define_gsubr ("set-bullet-hitbox!", 5, 0, 0, set_bullet_hitbox);
    scm_c_define_gsubr ("change-bullet-direction!", 2, 0, 0, change_bullet_direction);

    scm_c_export ("make-bullet", NULL);
    scm_c_export ("kill-bullet", NULL);
    scm_c_export ("bullet-x", NULL);
    scm_c_export ("bullet-y", NULL);
    scm_c_export ("bullet-y", NULL);
    scm_c_export ("bullet-speed", NULL);
    scm_c_export ("bullet-direction", NULL);
    scm_c_export ("bullet-acceleration", NULL);
    scm_c_export ("bullet-angular-velocity", NULL);
    scm_c_export ("set-bullet-position!", NULL);
    scm_c_export ("set-bullet-speed!", NULL);
    scm_c_export ("set-bullet-direction!", NULL);
    scm_c_export ("set-bullet-acceleration!", NULL);
    scm_c_export ("set-bullet-angular-velocity!", NULL);
    scm_c_export ("set-bullet-sprite!", NULL);
    scm_c_export ("set-bullet-hitbox!", NULL);
    scm_c_export ("change-bullet-direction!", NULL);

}
