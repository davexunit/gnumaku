#include "sprite.h"

static scm_t_bits sprite_tag;

static Sprite*
check_sprite (SCM sprite_smob) {
    scm_assert_smob_type (sprite_tag, sprite_smob);

    return (Sprite *) SCM_SMOB_DATA (sprite_smob);
}

static SCM
make_sprite (SCM image_smob, SCM s_pos, SCM s_scale, SCM s_rotation,
             SCM s_color, SCM s_anchor) {
    SCM smob;
    Sprite *sprite;
    Image *image = check_image (image_smob);
    Vector2 pos = scm_to_vector2 (s_pos);
    Vector2 scale = scm_to_vector2 (s_scale);
    float rotation = scm_to_double (s_rotation);
    ALLEGRO_COLOR color = scm_to_color (s_color);

    /* Step 1: Allocate the memory block.
     */
    sprite = (Sprite *) scm_gc_malloc (sizeof (Sprite), "sprite");

    /* Step 2: Initialize it with straight code.
     */
    sprite->image = SCM_BOOL_F;
    sprite->color = color;
    sprite->position = pos;
    sprite->scale = scale;
    sprite->rotation = rotation;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, sprite_tag, sprite);

    sprite->image = image_smob;

    /* Center sprite if no center position is given. */
    if (s_anchor == SCM_BOOL_F) {
        sprite->anchor = vector2_new (get_image_width (image) / 2,
                                      get_image_height (image) / 2);
    } else {
        sprite->anchor = scm_to_vector2 (s_anchor);
    }

    return smob;
}

static SCM
sprite_image (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return sprite->image;
}

static SCM
sprite_position (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_vector2 (sprite->position);
}

static SCM
sprite_scale (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_vector2 (sprite->scale);
}

static SCM
sprite_rotation (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->rotation);
}

static SCM
set_sprite_position (SCM sprite_smob, SCM s_pos) {
    Sprite *sprite = check_sprite (sprite_smob);
    Vector2 pos = scm_to_vector2 (s_pos);

    sprite->position = pos;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_scale (SCM sprite_smob, SCM s_scale) {
    Sprite *sprite = check_sprite (sprite_smob);
    Vector2 scale = scm_to_vector2 (s_scale);

    sprite->scale = scale;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_rotation (SCM sprite_smob, SCM s_rotation) {
    Sprite *sprite = check_sprite (sprite_smob);
    float rotation = scm_to_double (s_rotation);

    sprite->rotation = rotation;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_color (SCM sprite_smob, SCM s_color) {
    Sprite *sprite = check_sprite (sprite_smob);
    ALLEGRO_COLOR color = scm_to_color (s_color);

    sprite->color = color;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_image (SCM sprite_smob, SCM image_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    sprite->image = image_smob;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
sprite_draw (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);
    Image *image = check_image (sprite->image);

    al_draw_tinted_scaled_rotated_bitmap (image->bitmap, sprite->color,
                                          sprite->anchor.x, sprite->anchor.y,
                                          sprite->position.x, sprite->position.y,
                                          sprite->scale.x, sprite->scale.y,
                                          sprite->rotation, 0);

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
mark_sprite (SCM sprite_smob) {
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);

    return sprite->image;
}

static size_t
free_sprite (SCM sprite_smob) {
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);

    scm_gc_free (sprite, sizeof (Sprite), "sprite");

    return 0;
}

static int
print_sprite (SCM sprite_smob, SCM port, scm_print_state *pstate) {
    Sprite *sprite = (Sprite *) SCM_SMOB_DATA (sprite_smob);

    scm_puts ("#<Sprite ", port);
    scm_display (scm_from_double (sprite->position.x), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->position.y), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->scale.x), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->scale.y), port);
    scm_puts (" ", port);
    scm_display (scm_from_double (sprite->rotation), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

void
init_sprite_type (void) {
    sprite_tag = scm_make_smob_type ("Sprite", sizeof (Sprite));
    scm_set_smob_mark (sprite_tag, mark_sprite);
    scm_set_smob_free (sprite_tag, free_sprite);
    scm_set_smob_print (sprite_tag, print_sprite);

    scm_c_define_gsubr ("%make-sprite", 6, 0, 0, make_sprite);
    scm_c_define_gsubr ("sprite-image", 1, 0, 0, sprite_image);
    scm_c_define_gsubr ("sprite-position", 1, 0, 0, sprite_position);
    scm_c_define_gsubr ("sprite-scale", 1, 0, 0, sprite_scale);
    scm_c_define_gsubr ("sprite-rotation", 1, 0, 0, sprite_rotation);
    scm_c_define_gsubr ("set-sprite-image", 2, 0, 0, set_sprite_image);
    scm_c_define_gsubr ("set-sprite-position", 2, 0, 0, set_sprite_position);
    scm_c_define_gsubr ("set-sprite-scale", 2, 0, 0, set_sprite_scale);
    scm_c_define_gsubr ("set-sprite-rotation", 2, 0, 0, set_sprite_rotation);
    scm_c_define_gsubr ("set-sprite-color", 2, 0, 0, set_sprite_color);
    scm_c_define_gsubr ("draw-sprite", 1, 0, 0, sprite_draw);

    scm_c_export ("%make-sprite", NULL);
    scm_c_export ("sprite-image", NULL);
    scm_c_export ("sprite-position", NULL);
    scm_c_export ("sprite-scale", NULL);
    scm_c_export ("sprite-rotation", NULL);
    scm_c_export ("set-sprite-image", NULL);
    scm_c_export ("set-sprite-position",NULL); 
    scm_c_export ("set-sprite-scale", NULL);
    scm_c_export ("set-sprite-rotation", NULL);
    scm_c_export ("set-sprite-color", NULL);
    scm_c_export ("set-sprite-sheet", NULL);
    scm_c_export ("draw-sprite", NULL);
}
