#include "color.h"

static scm_t_bits color_tag;

ALLEGRO_COLOR gmk_color_mult_alpha (ALLEGRO_COLOR color)
{
    ALLEGRO_COLOR new_color;

    new_color.r = color.r * color.a;
    new_color.g = color.g * color.a;
    new_color.b = color.b * color.a;
    new_color.a = color.a;

    return new_color;
}

SCM
gmk_scm_from_color (ALLEGRO_COLOR color)
{
    ALLEGRO_COLOR *new_color = (ALLEGRO_COLOR *) scm_gc_malloc (sizeof (ALLEGRO_COLOR),
                                                               "color");

    *new_color = color;

    SCM_RETURN_NEWSMOB (color_tag, new_color);
}

ALLEGRO_COLOR
gmk_scm_to_color (SCM color)
{
    ALLEGRO_COLOR *c_color;

    scm_assert_smob_type (color_tag, color);
    c_color = (ALLEGRO_COLOR *) SCM_SMOB_DATA (color);

    return *c_color;
}

SCM_DEFINE (gmk_make_color, "make-color", 4, 0, 0,
            (SCM r, SCM g, SCM b, SCM a),
            "Make color from red, green, blue, and alpha components."
            "Each component is in the range [0, 255].")
{
    ALLEGRO_COLOR color = al_map_rgba (scm_to_int (r),
                                       scm_to_int (g),
                                       scm_to_int (b),
                                       scm_to_int (a));

    return gmk_scm_from_color (color);
}

SCM_DEFINE (gmk_make_color_f, "make-color-f", 4, 0, 0,
            (SCM r, SCM g, SCM b, SCM a),
            "Make color from red, green, blue, and alpha components."
            "Each component is in the range [0, 1].")
{
    ALLEGRO_COLOR color = al_map_rgba_f (scm_to_double (r),
                                         scm_to_double (g),
                                         scm_to_double (b),
                                         scm_to_double (a));

    return gmk_scm_from_color (color);
}

SCM_DEFINE (gmk_color_r, "color-r", 1, 0, 0,
            (SCM color),
            "Return red color component.")
{
    ALLEGRO_COLOR c_color = gmk_scm_to_color (color);

    return scm_from_double (c_color.r);
}

SCM_DEFINE (gmk_color_g, "color-g", 1, 0, 0,
            (SCM color),
            "Return green color component.")
{
    ALLEGRO_COLOR c_color = gmk_scm_to_color (color);

    return scm_from_double (c_color.g);
}

SCM_DEFINE (gmk_color_b, "color-b", 1, 0, 0,
            (SCM color),
            "Return blue color component.")
{
    ALLEGRO_COLOR c_color = gmk_scm_to_color (color);

    return scm_from_double (c_color.b);
}

SCM_DEFINE (gmk_color_a, "color-a", 1, 0, 0,
            (SCM color),
            "Return alpha color component.")
{
    ALLEGRO_COLOR c_color = gmk_scm_to_color (color);

    return scm_from_double (c_color.a);
}

static size_t
free_color (SCM color_smob)
{
    ALLEGRO_COLOR *color = (ALLEGRO_COLOR *) SCM_SMOB_DATA (color_smob);

    scm_gc_free (color, sizeof (ALLEGRO_COLOR), "color");

    return 0;
}

static int
print_color (SCM color_smob, SCM port, scm_print_state *pstate)
{
    ALLEGRO_COLOR color = gmk_scm_to_color (color_smob);

    scm_puts ("#<color r:", port);
    scm_display (scm_from_double (color.r), port);
    scm_puts (" g: ", port);
    scm_display (scm_from_double (color.g), port);
    scm_puts (" b: ", port);
    scm_display (scm_from_double (color.b), port);
    scm_puts (" a: ", port);
    scm_display (scm_from_double (color.a), port);
    scm_puts (">", port);

    return 1;
}

void
init_color (void)
{
    color_tag = scm_make_smob_type ("Color", sizeof (ALLEGRO_COLOR));
    scm_set_smob_mark (color_tag, 0);
    scm_set_smob_free (color_tag, free_color);
    scm_set_smob_print (color_tag, print_color);

#include "color.x"

    scm_c_export (s_gmk_make_color,
                  s_gmk_make_color_f,
                  s_gmk_color_r,
                  s_gmk_color_g,
                  s_gmk_color_b,
                  s_gmk_color_a,
                  NULL);
}
