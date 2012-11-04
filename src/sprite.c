#include "sprite.h"

static scm_t_bits sprite_tag;

static Sprite*
check_sprite (SCM sprite_smob)
{
    scm_assert_smob_type (sprite_tag, sprite_smob);
     
    return (Sprite *) SCM_SMOB_DATA (sprite_smob);
}

static SCM
make_sprite ()
{
    SCM smob;
    Sprite *sprite;

    /* Step 1: Allocate the memory block.
     */
    sprite = (Sprite *) scm_gc_malloc (sizeof (Sprite), "sprite");

    /* Step 2: Initialize it with straight code.
     */
    sprite->sprite_sheet_smob = SCM_BOOL_F;
    sprite->image = NULL;
    sprite->x = 0;
    sprite->y = 0;
    sprite->center_x = 0;
    sprite->center_y = 0;
    sprite->scale_x = 1;
    sprite->scale_y = 1;
    sprite->rotation = 0;
    sprite->color = al_map_rgba_f (1, 1, 1, 1);
     
    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, sprite_tag, sprite);

    return smob;

}

static SCM
sprite_sheet (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return sprite->sprite_sheet_smob;
}

static SCM
sprite_x (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->x);
}

static SCM
sprite_y (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->y);
}

static SCM
sprite_scale_x (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->scale_x);
}

static SCM
sprite_scale_y (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->scale_y);
}

static SCM
sprite_rotation (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->rotation);
}

static SCM
set_sprite_x (SCM sprite_smob, SCM s_x)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float x = scm_to_double (s_x);

    sprite->x = x;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_y (SCM sprite_smob, SCM s_y)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float y = scm_to_double (s_y);

    sprite->y = y;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_position (SCM sprite_smob, SCM s_x, SCM s_y)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    sprite->x = x;
    sprite->y = y;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_scale_x (SCM sprite_smob, SCM s_scale_x)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float scale_x = scm_to_double (s_scale_x);

    sprite->scale_x = scale_x;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_scale_y (SCM sprite_smob, SCM s_scale_y)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float scale_y = scm_to_double (s_scale_y);

    sprite->scale_y = scale_y;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_scale (SCM sprite_smob, SCM s_scale_x, SCM s_scale_y)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float scale_x = scm_to_double (s_scale_x);
    float scale_y = scm_to_double (s_scale_y);

    sprite->scale_x = scale_x;
    sprite->scale_y = scale_y;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_rotation (SCM sprite_smob, SCM s_rotation)
{
    Sprite *sprite = check_sprite (sprite_smob);
    float rotation = scm_to_double (s_rotation);

    sprite->rotation = rotation;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_sheet (SCM sprite_smob, SCM sprite_sheet_smob, SCM s_tile)
{
    Sprite *sprite = check_sprite (sprite_smob);
    SpriteSheet *sprite_sheet = check_sprite_sheet (sprite_sheet_smob);
    int tile = scm_to_int (s_tile);

    sprite->sprite_sheet_smob = sprite_sheet_smob;
    sprite->image = sprite_sheet->tiles[tile];
    sprite->center_x = al_get_bitmap_width (sprite->image) / 2;
    sprite->center_y = al_get_bitmap_height (sprite->image) / 2;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
sprite_draw (SCM sprite_smob)
{
    Sprite *sprite = check_sprite (sprite_smob);

    if (sprite->image)
    {
	al_draw_tinted_scaled_rotated_bitmap (sprite->image, sprite->color, sprite->center_x, sprite->center_y,
					      sprite->x, sprite->y, sprite->scale_x, sprite->scale_y,
					      sprite->rotation, 0);
    }

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
mark_sprite (SCM sprite_smob)
{
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);

    return sprite->sprite_sheet_smob;
}

static size_t
free_sprite (SCM sprite_smob)
{
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);
     
    scm_gc_free (sprite, sizeof (Sprite), "sprite");
     
    return 0;
}
     
static int
print_sprite (SCM sprite_smob, SCM port, scm_print_state *pstate)
{
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);
     
    scm_puts ("#<Sprite ", port);
    scm_display (scm_from_double (sprite->x), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->y), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->scale_x), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->scale_y), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->rotation), port);
    scm_puts (">", port);
     
    /* non-zero means success */
    return 1;
}

void
init_sprite_type (void)
{
    sprite_tag = scm_make_smob_type ("Sprite", sizeof (Sprite));
    scm_set_smob_mark (sprite_tag, mark_sprite);
    scm_set_smob_free (sprite_tag, free_sprite);
    scm_set_smob_print (sprite_tag, print_sprite);

    scm_c_define_gsubr ("make-sprite", 0, 0, 0, make_sprite);
    scm_c_define_gsubr ("sprite-sheet", 1, 0, 0, sprite_sheet);
    scm_c_define_gsubr ("sprite-x", 1, 0, 0, sprite_x);
    scm_c_define_gsubr ("sprite-y", 1, 0, 0, sprite_y);
    scm_c_define_gsubr ("sprite-scale-x", 1, 0, 0, sprite_scale_x);
    scm_c_define_gsubr ("sprite-scale-y", 1, 0, 0, sprite_scale_y);
    scm_c_define_gsubr ("sprite-rotation", 1, 0, 0, sprite_rotation);
    scm_c_define_gsubr ("set-sprite-x!", 2, 0, 0, set_sprite_x);
    scm_c_define_gsubr ("set-sprite-y!", 2, 0, 0, set_sprite_y);
    scm_c_define_gsubr ("set-sprite-position!", 3, 0, 0, set_sprite_position);
    scm_c_define_gsubr ("set-sprite-scale-x!", 2, 0, 0, set_sprite_scale_x);
    scm_c_define_gsubr ("set-sprite-scale-y!", 2, 0, 0, set_sprite_scale_y);
    scm_c_define_gsubr ("set-sprite-scale!", 3, 0, 0, set_sprite_scale);
    scm_c_define_gsubr ("set-sprite-rotation!", 2, 0, 0, set_sprite_rotation);
    scm_c_define_gsubr ("set-sprite-sheet!", 3, 0, 0, set_sprite_sheet);
    scm_c_define_gsubr ("draw-sprite", 1, 0, 0, sprite_draw);
}
