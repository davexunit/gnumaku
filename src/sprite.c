#include "sprite.h"

static scm_t_bits sprite_tag;

SCM_KEYWORD (keyword_position, "position");
SCM_KEYWORD (keyword_scale, "scale");
SCM_KEYWORD (keyword_rotation, "rotation");
SCM_KEYWORD (keyword_color, "color");
SCM_KEYWORD (keyword_anchor, "anchor");

static Sprite *
check_sprite (SCM sprite_smob) {
    scm_assert_smob_type (sprite_tag, sprite_smob);

    return (Sprite *) SCM_SMOB_DATA (sprite_smob);
}

SCM_DEFINE (make_sprite, "make-sprite", 1, 0, 1,
            (SCM image_smob, SCM kw_args),
            "Make a new sprite instance.")
{
    SCM smob;
    Sprite *sprite;
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image_smob);
    /*
     * Get optional arguments via a list of keywords.
     * If a keyword arg is not present, use a sane default.
     */
    Vector2 default_anchor = vector2_new (al_get_bitmap_width (bitmap) / 2,
                                          al_get_bitmap_height (bitmap) / 2);
    SCM s_position = scm_get_keyword (keyword_position, kw_args,
                                      scm_from_vector2 (vector2_new (0, 0)));
    SCM s_scale = scm_get_keyword (keyword_scale, kw_args,
                                   scm_from_vector2 (vector2_new (1, 1)));
    SCM s_anchor = scm_get_keyword (keyword_anchor, kw_args,
                                    scm_from_vector2 (default_anchor));
    SCM s_rotation = scm_get_keyword (keyword_rotation, kw_args, scm_from_double (0));
    SCM s_color = scm_get_keyword (keyword_color, kw_args,
                                   scm_from_color (al_map_rgba_f (1, 1, 1, 1)));

    sprite = (Sprite *) scm_gc_malloc (sizeof (Sprite), "sprite");
    sprite->image = SCM_BOOL_F;
    sprite->position = scm_to_vector2 (s_position);
    sprite->anchor = scm_to_vector2 (s_anchor);
    sprite->scale = scm_to_vector2 (s_scale);
    sprite->rotation = scm_to_double (s_rotation);
    sprite->color = scm_to_color (s_color);

    SCM_NEWSMOB (smob, sprite_tag, sprite);

    sprite->image = image_smob;

    return smob;
}

SCM_DEFINE (sprite_image, "sprite-image", 1, 0, 0,
            (SCM sprite_smob),
            "Return the sprite image.")
{
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
sprite_color (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_color (sprite->color);
}

static SCM
sprite_opacity (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_double (sprite->color.a);
}

static SCM
sprite_anchor (SCM sprite_smob) {
    Sprite *sprite = check_sprite (sprite_smob);

    scm_remember_upto_here_1 (sprite_smob);

    return scm_from_vector2 (sprite->anchor);
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
set_sprite_opacity (SCM sprite_smob, SCM s_opacity) {
    Sprite *sprite = check_sprite (sprite_smob);
    float opacity = scm_to_double (s_opacity);

    sprite->color.a = opacity;

    scm_remember_upto_here_1 (sprite_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_sprite_anchor (SCM sprite_smob, SCM s_anchor) {
    Sprite *sprite = check_sprite (sprite_smob);
    Vector2 anchor = scm_to_vector2 (s_anchor);

    sprite->anchor = anchor;

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
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (sprite->image);

    al_draw_tinted_scaled_rotated_bitmap (bitmap,
                                          color_mult_alpha (sprite->color),
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
    sprite_tag = scm_make_smob_type ("<sprite>", sizeof (Sprite));
    scm_set_smob_mark (sprite_tag, mark_sprite);
    scm_set_smob_free (sprite_tag, free_sprite);
    scm_set_smob_print (sprite_tag, print_sprite);

#include "sprite.x"
    scm_c_define_gsubr ("sprite-image", 1, 0, 0, sprite_image);
    scm_c_define_gsubr ("sprite-position", 1, 0, 0, sprite_position);
    scm_c_define_gsubr ("sprite-scale", 1, 0, 0, sprite_scale);
    scm_c_define_gsubr ("sprite-rotation", 1, 0, 0, sprite_rotation);
    scm_c_define_gsubr ("sprite-color", 1, 0, 0, sprite_color);
    scm_c_define_gsubr ("sprite-opacity", 1, 0, 0, sprite_opacity);
    scm_c_define_gsubr ("sprite-anchor", 1, 0, 0, sprite_anchor);
    scm_c_define_gsubr ("set-sprite-image", 2, 0, 0, set_sprite_image);
    scm_c_define_gsubr ("set-sprite-position", 2, 0, 0, set_sprite_position);
    scm_c_define_gsubr ("set-sprite-scale", 2, 0, 0, set_sprite_scale);
    scm_c_define_gsubr ("set-sprite-rotation", 2, 0, 0, set_sprite_rotation);
    scm_c_define_gsubr ("set-sprite-color", 2, 0, 0, set_sprite_color);
    scm_c_define_gsubr ("set-sprite-opacity", 2, 0, 0, set_sprite_opacity);
    scm_c_define_gsubr ("set-sprite-anchor", 2, 0, 0, set_sprite_anchor);
    scm_c_define_gsubr ("draw-sprite", 1, 0, 0, sprite_draw);

    scm_c_export ("make-sprite",
                  "sprite-image",
                  "sprite-position",
                  "sprite-scale",
                  "sprite-rotation",
                  "sprite-color",
                  "sprite-opacity",
                  "sprite-anchor",
                  "set-sprite-image",
                  "set-sprite-position",
                  "set-sprite-scale",
                  "set-sprite-rotation",
                  "set-sprite-color",
                  "set-sprite-opacity",
                  "set-sprite-anchor",
                  "set-sprite-sheet",
                  "draw-sprite",
                  NULL);
}
