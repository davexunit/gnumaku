#include "color.h"

ALLEGRO_COLOR color_mult_alpha (ALLEGRO_COLOR color) {
    ALLEGRO_COLOR new_color;
    
    new_color.r = color.r * color.a;
    new_color.g = color.g * color.a;
    new_color.b = color.b * color.a;
    new_color.a = color.a;

    return new_color;
}

static scm_t_bits color_tag;

SCM
scm_from_color (ALLEGRO_COLOR al_color) {

    SCM smob;
    Color *color = (Color *) scm_gc_malloc (sizeof (Color), "color");

    color->color = al_color;

    SCM_NEWSMOB (smob, color_tag, color);

    return smob;
}

ALLEGRO_COLOR
scm_to_color (SCM s_color) {
    Color *color;
    
    scm_assert_smob_type (color_tag, s_color);
    color = (Color *) SCM_SMOB_DATA (s_color);

    return color->color;
}

static SCM
make_color (SCM s_r, SCM s_g, SCM s_b, SCM s_a)
{
    int r = scm_to_int (s_r);
    int g = scm_to_int (s_g);
    int b = scm_to_int (s_b);
    int a = scm_to_int (s_a);

    return scm_from_color (al_map_rgba (r, g, b, a));
}

static SCM
make_color_f (SCM s_r, SCM s_g, SCM s_b, SCM s_a)
{
    float r = scm_to_double (s_r);
    float g = scm_to_double (s_g);
    float b = scm_to_double (s_b);
    float a = scm_to_double (s_a);

    return scm_from_color (al_map_rgba_f (r, g, b, a));
}

static size_t
free_color (SCM color_smob)
{
    Color *color = (Color *) SCM_SMOB_DATA (color_smob);

    scm_gc_free (color, sizeof (Color), "color");

    return 0;
}

static int
print_color (SCM color_smob, SCM port, scm_print_state *pstate)
{
    ALLEGRO_COLOR color = scm_to_color (color_smob);

    scm_puts ("#<color ", port);
    scm_display (scm_from_double (color.r), port);
    scm_puts (" r: ", port);
    scm_display (scm_from_double (color.g), port);
    scm_puts (" g: ", port);
    scm_display (scm_from_double (color.b), port);
    scm_puts (" b: ", port);
    scm_display (scm_from_double (color.a), port);
    scm_puts (" a: ", port);

    /* non-zero means success */
    return 1;
}

static SCM
color_r (SCM s_color) {
    ALLEGRO_COLOR color = scm_to_color (s_color);

    scm_remember_upto_here_1 (s_color);

    return scm_from_double (color.r);
}

static SCM
color_g (SCM s_color) {
    ALLEGRO_COLOR color = scm_to_color (s_color);

    scm_remember_upto_here_1 (s_color);

    return scm_from_double (color.g);
}

static SCM
color_b (SCM s_color) {
    ALLEGRO_COLOR color = scm_to_color (s_color);

    scm_remember_upto_here_1 (s_color);

    return scm_from_double (color.b);
}

static SCM
color_a (SCM s_color) {
    ALLEGRO_COLOR color = scm_to_color (s_color);

    scm_remember_upto_here_1 (s_color);

    return scm_from_double (color.a);
}

void
init_color_type (void)
{
    color_tag = scm_make_smob_type ("Color", sizeof (Color));
    scm_set_smob_mark (color_tag, 0);
    scm_set_smob_free (color_tag, free_color);
    scm_set_smob_print (color_tag, print_color);

    scm_c_define_gsubr ("make-color", 4, 0, 0, make_color);
    scm_c_define_gsubr ("make-color-f", 4, 0, 0, make_color_f);
    scm_c_define_gsubr ("color-r", 1, 0, 0, color_r);
    scm_c_define_gsubr ("color-g", 1, 0, 0, color_g);
    scm_c_define_gsubr ("color-b", 1, 0, 0, color_b);
    scm_c_define_gsubr ("color-a", 1, 0, 0, color_a);

    scm_c_export ("make-color", NULL);
    scm_c_export ("make-color-f", NULL);
    scm_c_export ("color-r", NULL);
    scm_c_export ("color-g", NULL);
    scm_c_export ("color-b", NULL);
    scm_c_export ("color-a", NULL);
}
