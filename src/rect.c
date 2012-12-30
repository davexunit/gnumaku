#include "rect.h"

static scm_t_bits rect_tag;

Rect
rect_new (float x, float y, float width, float height) {
    Rect rect;
    
    rect.x = x;
    rect.y = y;
    rect.width = width;
    rect.height = height;

    return rect;
}

bool rect_collide_point (Rect rect, Vector2 p) {
    return p.x >= rect.x && p.x < rect.x + rect.width &&
	   p.y >= rect.y && p.y < rect.y + rect.height;
}

bool rect_collide_rect (Rect rect, Rect other) {
    return ((rect.x >= other.x && rect.x < other.x + other.width) ||
	    (other.x >= rect.x && other.x < rect.x + rect.width)) &&
	((rect.y >= other.y && rect.y < other.y + other.height) ||
	 (other.y >= rect.y && other.y < rect.y + rect.height));
}

Vector2
get_rect_center (Rect rect) {
    return vector2_new (rect.x + rect.width / 2,
                        rect.y + rect.height / 2);
}

Rect
rect_center (Rect rect, Vector2 center) {
    Rect new_rect;

    new_rect.x = center.x - rect.width / 2;
    new_rect.y = center.y - rect.height / 2;
    new_rect.width = rect.width;
    new_rect.height = rect.height;

    return new_rect;
}

Rect
rect_move (Rect rect, Vector2 delta) {
    Rect new_rect;

    new_rect.x = rect.x + delta.x;
    new_rect.y = rect.y + delta.y;
    new_rect.width = rect.width;
    new_rect.height = rect.height;

    return new_rect;
}

Rect
rect_scale (Rect rect, Vector2 scale) {
    Rect scale_rect = rect;
    Vector2 center = get_rect_center (rect);

    scale_rect.width *= scale.x;
    scale_rect.height *= scale.y;

    return rect_center (scale_rect, center);
}

Rect*
check_rect (SCM rect_smob) {
    scm_assert_smob_type (rect_tag, rect_smob);

    return (Rect *) SCM_SMOB_DATA (rect_smob);
}

Rect scm_to_rect (SCM rect_smob) {
    Rect *rect = check_rect (rect_smob);
    
    return *rect;
}

SCM scm_from_rect (Rect rect) {
    SCM smob;
    Rect *new_rect;

    new_rect = (Rect *) scm_gc_malloc (sizeof (Rect), "rect");
    new_rect->x = rect.x;
    new_rect->y = rect.y;
    new_rect->width = rect.width;
    new_rect->height = rect.height;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, rect_tag, new_rect);

    return smob;
}

SCM
make_rect (SCM s_x, SCM s_y, SCM s_width, SCM s_height) {
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float width = scm_to_double (s_width);
    float height = scm_to_double (s_height);

    return scm_from_rect (rect_new (x, y, width, height));
}

static SCM
rect_x (SCM rect_smob) {
    Rect rect = scm_to_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect.x);
}

static SCM
rect_y (SCM rect_smob) {
    Rect rect = scm_to_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect.y);
}


static SCM
rect_width (SCM rect_smob) {
    Rect rect = scm_to_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect.width);
}

static SCM
rect_height (SCM rect_smob) {
    Rect rect = scm_to_rect (rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_double (rect.height);
}

static SCM
scm_rect_move (SCM rect_smob, SCM s_delta) {
    Rect rect = scm_to_rect (rect_smob);
    Vector2 delta = scm_to_vector2 (s_delta);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_rect (rect_move (rect, delta));
}

static SCM
scm_rect_center (SCM rect_smob, SCM s_p) {
    Rect rect = scm_to_rect (rect_smob);
    Vector2 p = scm_to_vector2 (s_p);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_rect (rect_center (rect, p));
}

static SCM
scm_rect_collide_point (SCM rect_smob, SCM s_p) {
    Rect rect = scm_to_rect (rect_smob);
    Vector2 p = scm_to_vector2 (s_p);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_bool (rect_collide_point (rect, p));
}


static SCM
scm_rect_collide_rect (SCM rect_smob, SCM other_rect_smob) {
    Rect rect = scm_to_rect (rect_smob);
    Rect other_rect = scm_to_rect (other_rect_smob);

    scm_remember_upto_here_1 (rect_smob);

    return scm_from_bool (rect_collide_rect (rect, other_rect));
}

static size_t
free_rect (SCM rect_smob) {
    Rect *rect = (Rect *) SCM_SMOB_DATA (rect_smob);

    scm_gc_free (rect, sizeof (Rect), "rect");

    return 0;
}

static int
print_rect (SCM rect_smob, SCM port, scm_print_state *pstate) {
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
init_rect_type (void) {
    rect_tag = scm_make_smob_type ("Rect", sizeof (Rect));
    scm_set_smob_mark (rect_tag, 0);
    scm_set_smob_free (rect_tag, free_rect);
    scm_set_smob_print (rect_tag, print_rect);

    scm_c_define_gsubr ("make-rect", 4, 0, 0, make_rect);
    scm_c_define_gsubr ("rect-x", 1, 0, 0, rect_x);
    scm_c_define_gsubr ("rect-y", 1, 0, 0, rect_y);
    scm_c_define_gsubr ("rect-width", 1, 0, 0, rect_width);
    scm_c_define_gsubr ("rect-height", 1, 0, 0, rect_height);
    scm_c_define_gsubr ("rect-move", 2, 0, 0, scm_rect_move);
    scm_c_define_gsubr ("rect-center", 2, 0, 0, scm_rect_center);
    scm_c_define_gsubr ("rect-collide-point", 2, 0, 0, scm_rect_collide_point);
    scm_c_define_gsubr ("rect-collide-rect", 2, 0, 0, scm_rect_collide_rect);

    scm_c_export ("make-rect", NULL);
    scm_c_export ("rect-x", NULL);
    scm_c_export ("rect-y", NULL);
    scm_c_export ("rect-width", NULL);
    scm_c_export ("rect-height", NULL);
    scm_c_export ("rect-move", NULL);
    scm_c_export ("rect-center", NULL);
    scm_c_export ("rect-collide-point", NULL);
    scm_c_export ("rect-collide-rect", NULL);
}
