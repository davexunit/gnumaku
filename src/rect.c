#include "rect.h"

static scm_t_bits rect_tag;

void
init_rect(Rect *rect, float x, float y, float width, float height)
{
    rect->x = x;
    rect->y = y;
    rect->width = x + width;
    rect->height = y + height;
}

bool rect_collide_point (Rect *rect, float x, float y)
{
    return x >= rect->x && x < rect->x + rect->width &&
	   y >= rect->y && y < rect->y + rect->height;
}

bool rect_collide_rect (Rect *rect, Rect *other)
{
    return ((rect->x >= other->x && rect->x < other->x + other->width) ||
	    (other->x >= rect->x && other->x < rect->x + rect->width)) &&
	((rect->y >= other->y && rect->y < other->y + other->height) ||
	 (other->y >= rect->y && other->y < rect->y + rect->height));
}

Rect
rect_move (Rect *rect, float dx, float dy)
{
    Rect new_rect;

    new_rect.x = rect->x + dx;
    new_rect.y = rect->y + dy;
    new_rect.width = rect->width;
    new_rect.height = rect->height;

    return new_rect;
}

Rect*
check_rect (SCM rect_smob)
{
    scm_assert_smob_type (rect_tag, rect_smob);

    return (Rect *) SCM_SMOB_DATA (rect_smob);
}

Rect scm_to_rect (SCM rect_smob) {
    Rect *rect = check_rect (rect_smob);
    
    return *rect;
}

SCM
make_rect (SCM s_x, SCM s_y, SCM s_width, SCM s_height)
{
    SCM smob;
    Rect *rect;
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float width = scm_to_double (s_width);
    float height = scm_to_double (s_height);

    /* Step 1: Allocate the memory block.
     */
    rect = (Rect *) scm_gc_malloc (sizeof (Rect), "rect");

    /* Step 2: Initialize it with straight code.
     */
    rect->x = x;
    rect->y = y;
    rect->width = width;
    rect->height = height;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, rect_tag, rect);

    return smob;
}

static SCM
rect_x (SCM rect_smob)
{
    Rect *rect = check_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect->x);
}

static SCM
rect_y (SCM rect_smob)
{
    Rect *rect = check_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect->y);
}


static SCM
rect_width (SCM rect_smob)
{
    Rect *rect = check_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect->width);
}

static SCM
rect_height (SCM rect_smob)
{
    Rect *rect = check_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect->height);
}

static SCM
set_rect_x (SCM rect_smob, SCM s_x)
{
    Rect *rect = check_rect (rect_smob);
    float x = scm_to_double (s_x);

    rect->x = x;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_rect_y (SCM rect_smob, SCM s_y)
{
    Rect *rect = check_rect (rect_smob);
    float y = scm_to_double (s_y);

    rect->y = y;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_rect_position (SCM rect_smob, SCM s_x, SCM s_y)
{
    Rect *rect = check_rect (rect_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    rect->x = x;
    rect->y = y;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_rect_width (SCM rect_smob, SCM s_width)
{
    Rect *rect = check_rect (rect_smob);
    float width = scm_to_double (s_width);

    rect->width = width;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_rect_height (SCM rect_smob, SCM s_height)
{
    Rect *rect = check_rect (rect_smob);
    float height = scm_to_double (s_height);

    rect->height = height;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
set_rect_size (SCM rect_smob, SCM s_width, SCM s_height)
{
    Rect *rect = check_rect (rect_smob);
    float width = scm_to_double (s_width);
    float height = scm_to_double (s_height);

    rect->width = width;
    rect->height = height;

    scm_remember_upto_here_1 (rect_smob);

    return SCM_UNSPECIFIED;
}

static SCM
scm_rect_move (SCM rect_smob, SCM s_dx, SCM s_dy)
{
    Rect *rect = check_rect (rect_smob);
    float dx = scm_to_double (s_dx);
    float dy = scm_to_double (s_dy);
    SCM x = scm_from_double (rect->x + dx);
    SCM y = scm_from_double (rect->y + dy);
    SCM width = scm_from_double (rect->width);
    SCM height = scm_from_double (rect->height);
    SCM new_rect = make_rect (x, y, width, height);

    scm_remember_upto_here_1 (rect_smob);

    return new_rect;
}

static SCM
scm_rect_collide_point (SCM rect_smob, SCM s_x, SCM s_y)
{
    Rect *rect = check_rect (rect_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_bool (rect_collide_point (rect, x, y));
}


static SCM
scm_rect_collide_rect (SCM rect_smob, SCM other_rect_smob)
{
    Rect *rect = check_rect (rect_smob);
    Rect *other_rect = check_rect (other_rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_bool (rect_collide_rect (rect, other_rect));
}

static size_t
free_rect (SCM rect_smob)
{
    Rect *rect = (Rect *) SCM_SMOB_DATA (rect_smob);

    scm_gc_free (rect, sizeof (Rect), "rect");

    return 0;
}

static int
print_rect (SCM rect_smob, SCM port, scm_print_state *pstate)
{
    Rect *rect = (Rect *) SCM_SMOB_DATA (rect_smob);

    scm_puts ("#<Rect x: ", port);
    scm_display (scm_from_double (rect->x), port);
    scm_puts (" y: ", port);
    scm_display (scm_from_double (rect->y), port);
    scm_puts (" width: ", port);
    scm_display (scm_from_double (rect->height), port);
    scm_puts (" height: ", port);
    scm_display (scm_from_double (rect->width), port);
    scm_puts (">", port);

    /* non-zero means success */
    return 1;
}

void
init_rect_type (void)
{
    rect_tag = scm_make_smob_type ("Rect", sizeof (Rect));
    scm_set_smob_mark (rect_tag, 0);
    scm_set_smob_free (rect_tag, free_rect);
    scm_set_smob_print (rect_tag, print_rect);

    scm_c_define_gsubr ("make-rect", 4, 0, 0, make_rect);
    scm_c_define_gsubr ("rect-x", 1, 0, 0, rect_x);
    scm_c_define_gsubr ("rect-y", 1, 0, 0, rect_y);
    scm_c_define_gsubr ("rect-width", 1, 0, 0, rect_width);
    scm_c_define_gsubr ("rect-height", 1, 0, 0, rect_height);
    scm_c_define_gsubr ("set-rect-x!", 2, 0, 0, set_rect_x);
    scm_c_define_gsubr ("set-rect-y!", 2, 0, 0, set_rect_y);
    scm_c_define_gsubr ("set-rect-position!", 3, 0, 0, set_rect_position);
    scm_c_define_gsubr ("set-rect-width!", 2, 0, 0, set_rect_width);
    scm_c_define_gsubr ("set-rect-height!", 2, 0, 0, set_rect_height);
    scm_c_define_gsubr ("set-rect-size!", 3, 0, 0, set_rect_size);
    scm_c_define_gsubr ("rect-move", 3, 0, 0, scm_rect_move);
    scm_c_define_gsubr ("rect-collide-point", 3, 0, 0, scm_rect_collide_point);
    scm_c_define_gsubr ("rect-collide-rect", 2, 0, 0, scm_rect_collide_rect);

    scm_c_export ("make-rect", NULL);
    scm_c_export ("rect-x", NULL);
    scm_c_export ("rect-y", NULL);
    scm_c_export ("rect-width", NULL);
    scm_c_export ("rect-height", NULL);
    scm_c_export ("set-rect-x!", NULL);
    scm_c_export ("set-rect-y!", NULL);
    scm_c_export ("set-rect-position!",NULL); 
    scm_c_export ("set-rect-width!", NULL);
    scm_c_export ("set-rect-height!", NULL);
    scm_c_export ("set-rect-size!", NULL);
    scm_c_export ("rect-move", NULL);
    scm_c_export ("rect-collide-point", NULL);
    scm_c_export ("rect-collide-rect", NULL);
}
