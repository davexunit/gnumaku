#include "vector.h"
#include <math.h>

bool vector2_equal (Vector2 a, Vector2 b) {
    return a.x == b.x && a.y == b.y;
}

float vector2_mag (Vector2 v) {
    return sqrt (v.x * v.x + v.y * v.y);
}

float vector2_dot (Vector2 a, Vector2 b) {
    return a.x * b.x + a.y * b.y;
}

float vector2_cross (Vector2 a, Vector2 b) {
    return a.x * b.y - b.x * a.y;
}

float vector2_angle (Vector2 v) {
    return atan2 (v.y, v.x);
}

Vector2 vector2_zero () {
    Vector2 v;

    v.x = 0;
    v.y = 0;

    return v;
}

Vector2 vector2_new (float x, float y) {
    Vector2 v;

    v.x = x;
    v.y = y;

    return v;
}

Vector2 vector2_add (Vector2 a, Vector2 b) {
    Vector2 c;

    c.x = a.x + b.x;
    c.y = a.y + b.y;

    return c;
}

Vector2 vector2_sub (Vector2 a, Vector2 b) {
    Vector2 c;

    c.x = a.x - b.x;
    c.y = a.y - b.y;

    return c;
}

Vector2 vector2_scale (Vector2 v, float scalar) {
    Vector2 a;

    a.x = v.x * scalar;
    a.y = v.y * scalar;

    return a;
}

Vector2 vector2_norm (Vector2 v) {
    float m = vector2_mag (v);
    Vector2 n = vector2_zero ();

    if (m != 0) {
        n.x = v.x / m;
        n.y = v.y / m;
    }

    return n;
}

Vector2 vector2_from_polar (float radius, float angle) {
    Vector2 v;

    v.x = cos (angle) * radius;
    v.y = sin (angle) * radius;

    return v;
}

Vector2 vector2_right_normal (Vector2 v) {
    Vector2 right;

    right.x = -v.y;
    right.y = v.x;

    return right;
}

Vector2 vector2_left_normal (Vector2 v) {
    Vector2 left;

    left.x = v.y;
    left.y = -v.x;

    return left;
}

static scm_t_bits vector2_tag;

static Vector2*
check_vector2 (SCM vector2_smob)
{
    scm_assert_smob_type (vector2_tag, vector2_smob);

    return (Vector2 *) SCM_SMOB_DATA (vector2_smob);
}

static SCM
make_vector2 (SCM s_x, SCM s_y)
{
    SCM smob;
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    Vector2 *vector2 = (Vector2 *) scm_gc_malloc (sizeof (Vector2), "vector2");

    vector2->x = x;
    vector2->y = y;

    SCM_NEWSMOB (smob, vector2_tag, vector2);

    return smob;
}

static size_t
free_vector2 (SCM vector2_smob)
{
    Vector2 *vector2 = (Vector2 *) SCM_SMOB_DATA (vector2_smob);

    scm_gc_free (vector2, sizeof (Vector2), "vector2");

    return 0;
}

static int
print_vector2 (SCM vector2_smob, SCM port, scm_print_state *pstate)
{
    Vector2 *vector2 = (Vector2 *) SCM_SMOB_DATA (vector2_smob);

    scm_puts ("#<vector2 x: ", port);
    scm_display (scm_from_double (vector2->x), port);
    scm_puts (" y: ", port);
    scm_display (scm_from_double (vector2->y), port);
    scm_puts (" >", port);
    
    /* non-zero means success */
    return 1;
}

void
init_vector2_type (void) {
    vector2_tag = scm_make_smob_type ("<vector2>", sizeof (Vector2));
    scm_set_smob_mark (vector2_tag, 0);
    scm_set_smob_free (vector2_tag, free_vector2);
    scm_set_smob_print (vector2_tag, print_vector2);

    scm_c_define_gsubr ("make-vector2", 2, 0, 0, make_vector2);

    scm_c_export ("make-vector2", NULL);
}
