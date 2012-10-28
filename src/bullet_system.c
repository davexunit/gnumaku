#include <libguile.h>

#include "bullet_system.h"

static scm_t_bits bullet_system_tag;

static BulletSystem*
check_bullet_system (SCM bullet_system_smob)
{
    scm_assert_smob_type (bullet_system_tag, bullet_system_smob);
     
    return (BulletSystem *) SCM_SMOB_DATA (bullet_system_smob);
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
	if (!get_bullet_at_index(bullet_system, i)->alive)
	    return i;
    }

    return -1;
}

static SCM
make_bullet (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);

    int bullet_index = free_bullet_index(bullet_system);

    if (bullet_index != -1)
    {
	Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);
	init_bullet(bullet);
	bullet->alive = true;
    }

    scm_remember_upto_here_1 (bullet_system_smob);

    return scm_from_int(bullet_index);
}
     
static SCM
clear_bullet_system (SCM bullet_system_smob)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    
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
remove_out_of_bounds_bullet(Bullet *bullet)
{
    // hard-coded values for now
    static float border = 32;
    static float width = 800;
    static float height = 600;

    if (bullet->x < -border ||
	bullet->x > width + border ||
	bullet->y < -border ||
	bullet->y > height + border)
    {
	bullet->alive = false;
    }
}

static SCM
update_bullet_system (SCM bullet_system_smob, SCM s_dt)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    float dt = scm_to_double(s_dt);
    
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
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet(bullet_system->sprite_sheet);

    al_hold_bitmap_drawing(true);

    for (int i = 0; i < bullet_system->max_bullets; ++i)
    {
	Bullet *bullet = bullet_system->bullets + i;

	if (bullet->alive && bullet->image)
	{
	    al_draw_rotated_bitmap(bullet->image, sprite_sheet->tile_width / 2, sprite_sheet->tile_height / 2, bullet->x, bullet->y, deg2rad(bullet->direction), 0);
	}
    }

    al_hold_bitmap_drawing(false);
    scm_remember_upto_here_1 (bullet_system_smob);

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
set_bullet_position (SCM bullet_system_smob, SCM s_bullet_index, SCM s_x, SCM s_y)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    int bullet_index = scm_to_int(s_bullet_index);
    float x = scm_to_double(s_x);
    float y = scm_to_double(s_y);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->x = x;
    bullet->y = y;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_bullet_speed (SCM bullet_system_smob, SCM s_bullet_index, SCM s_speed)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    int bullet_index = scm_to_int(s_bullet_index);
    float speed = scm_to_double(s_speed);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->speed = speed;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;    
}

static SCM
set_bullet_acceleration (SCM bullet_system_smob, SCM s_bullet_index, SCM s_acceleration)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    int bullet_index = scm_to_int(s_bullet_index);
    float acceleration = scm_to_double(s_acceleration);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->acceleration = acceleration;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;    
}

static SCM
set_bullet_direction (SCM bullet_system_smob, SCM s_bullet_index, SCM s_direction)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    int bullet_index = scm_to_int(s_bullet_index);
    float direction = scm_to_double(s_direction);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->direction = direction;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;    
}

static SCM
set_bullet_angular_velocity (SCM bullet_system_smob, SCM s_bullet_index, SCM s_angular_velocity)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    int bullet_index = scm_to_int(s_bullet_index);
    float angular_velocity = scm_to_double(s_angular_velocity);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->angular_velocity = angular_velocity;

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;    
}

static SCM
set_bullet_sprite (SCM bullet_system_smob, SCM s_bullet_index, SCM s_sprite_index)
{
    BulletSystem *bullet_system = check_bullet_system(bullet_system_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet(bullet_system->sprite_sheet);
    int bullet_index = scm_to_int(s_bullet_index);
    int sprite_index = scm_to_int(s_sprite_index);
    Bullet *bullet = get_bullet_at_index(bullet_system, bullet_index);

    bullet->image = sprite_sheet->tiles[sprite_index];

    scm_remember_upto_here_1 (bullet_system_smob);

    return SCM_UNSPECIFIED;    
}
     
void
init_bullet_system_type (void)
{
    bullet_system_tag = scm_make_smob_type ("BulletSystem", sizeof (BulletSystem));
    scm_set_smob_mark (bullet_system_tag, mark_bullet_system);
    scm_set_smob_free (bullet_system_tag, free_bullet_system);
    scm_set_smob_print (bullet_system_tag, print_bullet_system);

    scm_c_define_gsubr ("make-bullet-system", 1, 0, 0, make_bullet_system);
    scm_c_define_gsubr ("clear-bullet-system", 1, 0, 0, clear_bullet_system);
    scm_c_define_gsubr ("draw-bullet-system", 1, 0, 0, draw_bullet_system);
    scm_c_define_gsubr ("update-bullet-system", 2, 0, 0, update_bullet_system);
    scm_c_define_gsubr ("set-bullet-system-sprite-sheet", 2, 0, 0, set_bullet_system_sprite_sheet);
    scm_c_define_gsubr ("make-bullet", 1, 0, 0, make_bullet);
    scm_c_define_gsubr ("set-bullet-position", 4, 0, 0, set_bullet_position);
    scm_c_define_gsubr ("set-bullet-speed", 3, 0, 0, set_bullet_speed);
    scm_c_define_gsubr ("set-bullet-direction", 3, 0, 0, set_bullet_direction);
    scm_c_define_gsubr ("set-bullet-acceleration", 3, 0, 0, set_bullet_acceleration);
    scm_c_define_gsubr ("set-bullet-angular-velocity", 3, 0, 0, set_bullet_angular_velocity);
    scm_c_define_gsubr ("set-bullet-sprite", 3, 0, 0, set_bullet_sprite);
}
