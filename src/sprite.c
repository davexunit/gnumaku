#include "sprite.h"

static scm_t_bits sprite_tag;

SCM_KEYWORD (keyword_position, "position");
SCM_KEYWORD (keyword_scale, "scale");
SCM_KEYWORD (keyword_rotation, "rotation");
SCM_KEYWORD (keyword_color, "color");
SCM_KEYWORD (keyword_anchor, "anchor");

static GmkSprite *
check_sprite (SCM sprite)
{
    scm_assert_smob_type (sprite_tag, sprite);

    return (GmkSprite *) SCM_SMOB_DATA (sprite);
}

GmkSprite
gmk_scm_to_sprite (SCM sprite)
{
    return *check_sprite (sprite);
}

SCM
gmk_scm_from_sprite (GmkSprite sprite)
{
    SCM smob;
    GmkSprite *new_sprite = (GmkSprite *) scm_gc_malloc (sizeof (GmkSprite), "sprite");

    new_sprite->image = SCM_BOOL_F;
    new_sprite->position = sprite.position;
    new_sprite->anchor = sprite.anchor;
    new_sprite->scale = sprite.scale;
    new_sprite->rotation = sprite.rotation;
    new_sprite->color = sprite.color;

    SCM_NEWSMOB (smob, sprite_tag, new_sprite);

    new_sprite->image = sprite.image;

    return smob;
}


SCM_DEFINE (gmk_make_sprite, "make-sprite", 1, 0, 1,
            (SCM image, SCM kw_args),
            "Make a new sprite instance.")
{
    SCM smob;
    GmkSprite *sprite;
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image);
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

    sprite = (GmkSprite *) scm_gc_malloc (sizeof (GmkSprite), "sprite");
    sprite->image = SCM_BOOL_F;
    sprite->position = scm_to_vector2 (s_position);
    sprite->anchor = scm_to_vector2 (s_anchor);
    sprite->scale = scm_to_vector2 (s_scale);
    sprite->rotation = scm_to_double (s_rotation);
    sprite->color = scm_to_color (s_color);

    SCM_NEWSMOB (smob, sprite_tag, sprite);

    sprite->image = image;

    return smob;
}

SCM_DEFINE (gmk_sprite_image, "sprite-image", 1, 0, 0,
            (SCM sprite),
            "Return the sprite image.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    SCM image = c_sprite->image;

    scm_remember_upto_here_1 (sprite);

    return image;
}

SCM_DEFINE (gmk_sprite_position, "sprite-position", 1, 0, 0,
            (SCM sprite),
            "Return sprite position vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 position = c_sprite->position;

    scm_remember_upto_here_1 (sprite);

    return scm_from_vector2 (position);
}

SCM_DEFINE (gmk_sprite_scale, "sprite-scale", 1, 0, 0,
            (SCM sprite),
            "Return sprite scale vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 scale = c_sprite->scale;

    scm_remember_upto_here_1 (sprite);

    return scm_from_vector2 (scale);
}

SCM_DEFINE (gmk_sprite_rotation, "sprite-rotation", 1, 0, 0,
            (SCM sprite),
            "Return sprite rotation angle.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    float rotation = c_sprite->rotation;

    scm_remember_upto_here_1 (sprite);

    return scm_from_double (rotation);
}

SCM_DEFINE (gmk_sprite_color, "sprite-color", 1, 0, 0,
            (SCM sprite),
            "Return sprite tint color.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    ALLEGRO_COLOR color = c_sprite->color;

    scm_remember_upto_here_1 (sprite);

    return scm_from_color (color);
}

SCM_DEFINE (gmk_sprite_opacity, "sprite-opacity", 1, 0, 0,
            (SCM sprite),
            "Return sprite tint opacity.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    float opacity = c_sprite->color.a;

    scm_remember_upto_here_1 (sprite);

    return scm_from_double (opacity);
}

SCM_DEFINE (gmk_sprite_anchor, "sprite-anchor", 1, 0, 0,
            (SCM sprite),
            "Return sprite anchor vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 anchor = c_sprite->anchor;

    scm_remember_upto_here_1 (sprite);

    return scm_from_vector2 (anchor);
}

SCM_DEFINE (gmk_set_sprite_position, "set-sprite-position", 2, 0, 0,
            (SCM sprite, SCM position),
            "Set sprite position vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 new_position = scm_to_vector2 (position);

    c_sprite->position = new_position;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_scale, "set-sprite-scale", 2, 0, 0,
            (SCM sprite, SCM scale),
            "Set sprite scale vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 new_scale = scm_to_vector2 (scale);

    c_sprite->scale = new_scale;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_rotation, "set-sprite-rotation", 2, 0, 0,
            (SCM sprite, SCM rotation),
            "Set sprite rotation angle.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    float new_rotation = scm_to_double (rotation);

    c_sprite->rotation = new_rotation;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_color, "set-sprite-color", 2, 0, 0,
            (SCM sprite, SCM color),
            "Set sprite color.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    ALLEGRO_COLOR new_color = scm_to_color (color);

    c_sprite->color = new_color;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_opacity, "set-sprite-opacity", 2, 0, 0,
            (SCM sprite, SCM opacity),
            "Set sprite opacity.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    float new_opacity = scm_to_double (opacity);

    c_sprite->color.a = new_opacity;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_anchor, "set-sprite-anchor", 2, 0, 0,
            (SCM sprite, SCM anchor),
            "Set sprite anchor vector.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    Vector2 new_anchor = scm_to_vector2 (anchor);

    c_sprite->anchor = new_anchor;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_sprite_image, "set-sprite-image", 2, 0, 0,
            (SCM sprite, SCM image),
            "Set sprite image.")
{
    GmkSprite *c_sprite = check_sprite (sprite);

    c_sprite->image = image;

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_draw_sprite, "draw-sprite", 1, 0, 0,
            (SCM sprite),
            "Draw sprite.")
{
    GmkSprite *c_sprite = check_sprite (sprite);
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (c_sprite->image);

    al_draw_tinted_scaled_rotated_bitmap (bitmap,
                                          color_mult_alpha (c_sprite->color),
                                          c_sprite->anchor.x, c_sprite->anchor.y,
                                          c_sprite->position.x, c_sprite->position.y,
                                          c_sprite->scale.x, c_sprite->scale.y,
                                          c_sprite->rotation, 0);

    scm_remember_upto_here_1 (sprite);

    return SCM_UNSPECIFIED;
}

static SCM
mark_sprite (SCM sprite)
{
    GmkSprite *c_sprite = (GmkSprite *) SCM_SMOB_DATA (sprite);

    return c_sprite->image;
}

static size_t
free_sprite (SCM sprite)
{
    GmkSprite *c_sprite = (GmkSprite *) SCM_SMOB_DATA (sprite);

    scm_gc_free (c_sprite, sizeof (GmkSprite), "sprite");

    return 0;
}

static int
print_sprite (SCM sprite, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<sprite position: ", port);
    scm_display (gmk_sprite_position (sprite), port);
    scm_puts (" scale: ", port);
    scm_display (gmk_sprite_scale (sprite), port);
    scm_puts (" anchor: ", port);
    scm_display (gmk_sprite_anchor (sprite), port);
    scm_puts (" rotation: ", port);
    scm_display (gmk_sprite_rotation (sprite), port);
    scm_puts (" color: ", port);
    scm_display (gmk_sprite_color (sprite), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

void
gmk_init_sprite (void)
{
    sprite_tag = scm_make_smob_type ("sprite", sizeof (GmkSprite));
    scm_set_smob_mark (sprite_tag, mark_sprite);
    scm_set_smob_free (sprite_tag, free_sprite);
    scm_set_smob_print (sprite_tag, print_sprite);

#include "sprite.x"

    scm_c_export (s_gmk_make_sprite,
                  s_gmk_sprite_image,
                  s_gmk_sprite_position,
                  s_gmk_sprite_anchor,
                  s_gmk_sprite_scale,
                  s_gmk_sprite_rotation,
                  s_gmk_sprite_color,
                  s_gmk_sprite_opacity,
                  s_gmk_set_sprite_image,
                  s_gmk_set_sprite_position,
                  s_gmk_set_sprite_anchor,
                  s_gmk_set_sprite_scale,
                  s_gmk_set_sprite_rotation,
                  s_gmk_set_sprite_color,
                  s_gmk_set_sprite_opacity,
                  s_gmk_draw_sprite,
                  NULL);
}
