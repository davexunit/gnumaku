#include "draw.h"
#include "rect.h"
#include "color.h"

SCM_DEFINE (gmk_s_clipping_rect, "clipping-rect", 0, 0, 0,
            (void),
            "Return current clipping rectangle.")
{
    int x, y, width, height;

    al_get_clipping_rectangle (&x, &y, &width, &height);

    return gmk_s_make_rect (scm_from_int (x), scm_from_int (y),
                            scm_from_int (width), scm_from_int (height));
}

SCM_DEFINE (gmk_s_set_clipping_rect, "set-clipping-rect", 1, 0, 0,
            (SCM rect),
            "Set clipping rectangle.")
{
    GmkRect r = gmk_scm_to_rect (rect);

    al_set_clipping_rectangle (r.x, r.y, r.width, r.height);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_reset_clipping_rect, "reset-clipping-rect", 0, 0, 0,
            (void),
            "Reset clipping rectangle to render buffer size.")
{
    al_reset_clipping_rectangle ();

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_draw_line, "draw-line", 6, 0, 0,
            (SCM x1, SCM y1, SCM x2, SCM y2, SCM color, SCM thickness),
            "Draw a straight line.")
{
    al_draw_line (scm_to_double (x1), scm_to_double (y1),
                  scm_to_double (x2), scm_to_double (y2),
                  gmk_scm_to_color (color), scm_to_double (thickness));

    return SCM_UNSPECIFIED;
}

void
gmk_init_draw (void)
{
#include "draw.x"

    scm_c_export (s_gmk_s_clipping_rect,
                  s_gmk_s_set_clipping_rect,
                  s_gmk_s_reset_clipping_rect,
                  s_gmk_s_draw_line,
                  NULL);
}
