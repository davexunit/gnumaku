#include "draw.h"
#include "rect.h"
#include "color.h"

static SCM
get_clipping_rect ()
{
    int x, y, width, height;

    al_get_clipping_rectangle (&x, &y, &width, &height);

    return gmk_make_rect (scm_from_int (x), scm_from_int (y), scm_from_int (width), scm_from_int (height));
}

static SCM
set_clipping_rect (SCM rect_smob)
{
    GmkRect rect = gmk_scm_to_rect (rect_smob);

    al_set_clipping_rectangle (rect.x, rect.y, rect.width, rect.height);

    return SCM_UNSPECIFIED;
}

static SCM
reset_clipping_rect ()
{
    al_reset_clipping_rectangle ();

    return SCM_UNSPECIFIED;
}

static SCM
draw_line (SCM s_x1, SCM s_y1, SCM s_x2, SCM s_y2, SCM s_color, SCM s_thickness)
{
    float x1 = scm_to_double (s_x1);
    float y1 = scm_to_double (s_y1);
    float x2 = scm_to_double (s_x2);
    float y2 = scm_to_double (s_y2);
    float thickness = scm_to_double (s_thickness);
    ALLEGRO_COLOR color = gmk_scm_to_color (s_color);

    al_draw_line (x1, y1, x2, y2, color, thickness);

    return SCM_UNSPECIFIED;
}

void
bind_draw_funcs ()
{
    scm_c_define_gsubr ("get-clipping-rect", 0, 0, 0, get_clipping_rect);
    scm_c_define_gsubr ("set-clipping-rect", 1, 0, 0, set_clipping_rect);
    scm_c_define_gsubr ("reset-clipping-rect", 0, 0, 0, reset_clipping_rect);
    scm_c_define_gsubr ("draw-line", 6, 0, 0, draw_line);

    scm_c_export ("get-clipping-rect", NULL);
    scm_c_export ("set-clipping-rect", NULL);
    scm_c_export ("reset-clipping-rect", NULL);
    scm_c_export ("draw-line", NULL);
}
