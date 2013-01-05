#include "rect.h"

static scm_t_bits rect_tag;

GmkRect
gmk_rect_new (float x, float y, float width, float height)
{
    GmkRect rect;

    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;

    return rect;
}

bool
gmk_rect_collide_point (GmkRect rect, GmkVector2 p)
{
    return p.x >= rect.x && p.x < rect.x + rect.width &&
        p.y >= rect.y && p.y < rect.y + rect.height;
}

bool
gmk_rect_collide_rect (GmkRect rect, GmkRect other)
{
    return ((rect.x >= other.x && rect.x < other.x + other.width) ||
            (other.x >= rect.x && other.x < rect.x + rect.width)) &&
        ((rect.y >= other.y && rect.y < other.y + other.height) ||
         (other.y >= rect.y && other.y < rect.y + rect.height));
}

GmkVector2
gmk_get_rect_center (GmkRect rect)
{
    return gmk_vector2_new (rect.x + rect.width / 2,
                            rect.y + rect.height / 2);
}

GmkRect
gmk_rect_center (GmkRect rect, GmkVector2 center)
{
    GmkRect new_rect;

    new_rect.x = center.x - rect.width / 2;
    new_rect.y = center.y - rect.height / 2;
    new_rect.width = rect.width;
    new_rect.height = rect.height;

    return new_rect;
}

GmkRect
gmk_rect_move (GmkRect rect, GmkVector2 delta)
{
    GmkRect new_rect;

    new_rect.x = rect.x + delta.x;
    new_rect.y = rect.y + delta.y;
    new_rect.width = rect.width;
    new_rect.height = rect.height;

    return new_rect;
}

GmkRect
gmk_rect_scale (GmkRect rect, GmkVector2 scale)
{
    GmkRect scale_rect = rect;
    GmkVector2 center = gmk_get_rect_center (rect);

    scale_rect.width *= scale.x;
    scale_rect.height *= scale.y;

    return gmk_rect_center (scale_rect, center);
}

GmkRect
gmk_scm_to_rect (SCM rect)
{
    scm_assert_smob_type (rect_tag, rect);

    return *((GmkRect *) SCM_SMOB_DATA (rect));
}

SCM
gmk_scm_from_rect (GmkRect rect) {
    GmkRect *new_rect;

    new_rect = (GmkRect *) scm_gc_malloc (sizeof (GmkRect), "rect");
    new_rect->x = rect.x;
    new_rect->y = rect.y;
    new_rect->width = rect.width;
    new_rect->height = rect.height;

    SCM_RETURN_NEWSMOB (rect_tag, new_rect);
}

SCM_DEFINE (gmk_s_make_rect, "make-rect", 4, 0, 0,
            (SCM x, SCM y, SCM width, SCM height),
            "Make rectangle.")
{
    return gmk_scm_from_rect (gmk_rect_new (scm_to_double (x),
                                            scm_to_double (y),
                                            scm_to_double (width),
                                            scm_to_double (height)));
}

SCM_DEFINE (gmk_s_rect_x, "rect-x", 1, 0, 0,
            (SCM rect),
            "Return x coordinate.")
{
    GmkRect r = gmk_scm_to_rect (rect);

    return scm_from_double (r.x);
}

SCM_DEFINE (gmk_s_rect_y, "rect-y", 1, 0, 0,
            (SCM rect),
            "Return y coordinate.")
{
    GmkRect r = gmk_scm_to_rect (rect);

    return scm_from_double (r.y);
}

SCM_DEFINE (gmk_s_rect_width, "rect-width", 1, 0, 0,
            (SCM rect),
            "Return width.")
{
    GmkRect r = gmk_scm_to_rect (rect);

    return scm_from_double (r.height);
}

SCM_DEFINE (gmk_s_rect_height, "rect-height", 1, 0, 0,
            (SCM rect),
            "Return height.")
{
    GmkRect r = gmk_scm_to_rect (rect);

    return scm_from_double (r.height);
}

SCM_DEFINE (gmk_s_rect_move, "rect-move", 2, 0, 0,
            (SCM rect, SCM delta),
            "Return a new rectangle translated by @var{delta} units.")
{
    return gmk_scm_from_rect (gmk_rect_move (gmk_scm_to_rect (rect),
                                             gmk_scm_to_vector2 (delta)));
}

SCM_DEFINE (gmk_s_rect_center, "rect-center", 2, 0, 0,
            (SCM rect, SCM center),
            "Center rectangle at @var{position}.")
{
    return gmk_scm_from_rect (gmk_rect_center (gmk_scm_to_rect (rect),
                                               gmk_scm_to_vector2 (center)));
}

SCM_DEFINE (gmk_s_rect_collide_point, "rect-collide-point", 2, 0, 0,
            (SCM rect, SCM point),
            "Return @code{#t} if @var{point} is within @var{rect}.")
{
    return scm_from_bool (gmk_rect_collide_point (gmk_scm_to_rect (rect),
                                                  gmk_scm_to_vector2 (point)));
}


SCM_DEFINE (gmk_s_rect_collide_rect, "rect-collide-rect", 2, 0, 0,
            (SCM rect1, SCM rect2),
            "Return @code{#t} if @var{rect1} overlaps @var{rect2}.")
{
    return scm_from_bool (gmk_rect_collide_rect (gmk_scm_to_rect (rect1),
                                                 gmk_scm_to_rect (rect2)));
}

static size_t
free_rect (SCM rect)
{
    GmkRect *c_rect = (GmkRect *) SCM_SMOB_DATA (rect);

    scm_gc_free (c_rect, sizeof (GmkRect), "rect");

    return 0;
}

static int
print_rect (SCM rect, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<rect x: ", port);
    scm_display (gmk_s_rect_x (rect), port);
    scm_puts (" y: ", port);
    scm_display (gmk_s_rect_y (rect), port);
    scm_puts (" width: ", port);
    scm_display (gmk_s_rect_width (rect), port);
    scm_puts (" height: ", port);
    scm_display (gmk_s_rect_height (rect), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_rect (void)
{
    rect_tag = scm_make_smob_type ("rect", sizeof (GmkRect));
    scm_set_smob_mark (rect_tag, 0);
    scm_set_smob_free (rect_tag, free_rect);
    scm_set_smob_print (rect_tag, print_rect);

#include "rect.x"

    scm_c_export (s_gmk_s_make_rect,
                  s_gmk_s_rect_x,
                  s_gmk_s_rect_y,
                  s_gmk_s_rect_width,
                  s_gmk_s_rect_height,
                  s_gmk_s_rect_move,
                  s_gmk_s_rect_center,
                  s_gmk_s_rect_collide_point,
                  s_gmk_s_rect_collide_rect,
                  NULL);
}
