#include "draw.h"
#include "rect.h"

static SCM
get_clipping_rect ()
{
    int x, y, width, height;
    
    al_get_clipping_rectangle (&x, &y, &width, &height);
    
    return make_rect (scm_from_int (x), scm_from_int (y), scm_from_int (width), scm_from_int (height));
}

static SCM
set_clipping_rect (SCM rect_smob)
{
    Rect *rect = check_rect (rect_smob);

    al_set_clipping_rectangle (rect->x, rect->y, rect->width, rect->height);

    return SCM_UNSPECIFIED;
}

static SCM
reset_clipping_rect ()
{
    al_reset_clipping_rectangle ();

    return SCM_UNSPECIFIED;
}

void
bind_draw_funcs ()
{
    scm_c_define_gsubr ("get-clipping-rect", 0, 0, 0, get_clipping_rect);
    scm_c_define_gsubr ("set-clipping-rect", 1, 0, 0, set_clipping_rect);
    scm_c_define_gsubr ("reset-clipping-rect", 0, 0, 0, reset_clipping_rect);

    scm_c_export ("get-clipping-rect", NULL);
    scm_c_export ("set-clipping-rect", NULL);
    scm_c_export ("reset-clipping-rect", NULL);
}
