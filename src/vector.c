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

Vector2
scm_to_vector2 (SCM s_v) {
    scm_assert_smob_type (vector2_tag, s_v);

    return *((Vector2 *) SCM_SMOB_DATA (s_v));
}

SCM scm_from_vector2 (Vector2 v) {
    SCM smob;
    Vector2 *new_v = (Vector2 *) scm_gc_malloc (sizeof (Vector2), "vector2");

    *new_v = v;

    SCM_NEWSMOB (smob, vector2_tag, new_v);

    return smob;
}

static SCM
make_vector2 (SCM s_x, SCM s_y)
{
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    return scm_from_vector2 (vector2_new (x, y));
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

static SCM
s_vector2_from_polar (SCM s_radius, SCM s_angle)
{
    float radius = scm_to_double (s_radius);
    float angle = scm_to_double (s_angle);
    Vector2 v = vector2_from_polar (radius, angle);

    return scm_from_vector2 (v);
}

static SCM
s_vector2_x (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);
    SCM x = scm_from_double (v.x);
    
    scm_remember_upto_here_1 (s_v);

    return x;
}

static SCM
s_vector2_y (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);
    SCM y = scm_from_double (v.y);
    
    scm_remember_upto_here_1 (s_v);

    return y;
}

static SCM
s_vector2_equalp (SCM s_v1, SCM s_v2) {
    Vector2 v1 = scm_to_vector2 (s_v1);
    Vector2 v2 = scm_to_vector2 (s_v2);
    
    scm_remember_upto_here_1 (s_v1);
    scm_remember_upto_here_1 (s_v2);
    
    return scm_from_bool (vector2_equal (v1, v2));
}

static SCM
s_vector2_add (SCM s_vectors) {
    Vector2 sum = vector2_zero ();

    while (!scm_to_bool (scm_null_p (s_vectors))) {
        SCM s_v = scm_car (s_vectors);
        Vector2 v = scm_to_vector2 (s_v);

        sum = vector2_add (sum, v);
        
        s_vectors = scm_cdr (s_vectors);
    }
    
    scm_remember_upto_here_1 (s_vectors);
    
    return scm_from_vector2 (sum);
}

static SCM
s_vector2_sub (SCM s_v, SCM s_vectors) {
    Vector2 difference = scm_to_vector2 (s_v);

    while (!scm_to_bool (scm_null_p (s_vectors))) {
        SCM s_v = scm_car (s_vectors);
        Vector2 v = scm_to_vector2 (s_v);

        difference = vector2_sub (difference, v);
        
        s_vectors = scm_cdr (s_vectors);
    }
    
    scm_remember_upto_here_1 (s_vectors);
    
    return scm_from_vector2 (difference);
}

static SCM
s_vector2_scale (SCM s_v, SCM s_scalar) {
    Vector2 v = scm_to_vector2 (s_v);
    float scalar = scm_to_double (s_scalar);
    
    scm_remember_upto_here_1 (s_v);
    
    return scm_from_vector2 (vector2_scale (v, scalar));
}

static SCM
s_vector2_norm (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);

    scm_remember_upto_here_1 (s_v);

    return scm_from_vector2 (vector2_norm (v));
}

static SCM
s_vector2_mag (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);

    scm_remember_upto_here_1 (s_v);

    return scm_from_double (vector2_mag (v));
}

static SCM
s_vector2_angle (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);

    scm_remember_upto_here_1 (s_v);

    return scm_from_double (vector2_angle (v));
}

static SCM
s_vector2_dot (SCM s_v1, SCM s_v2) {
    Vector2 v1 = scm_to_vector2 (s_v1);
    Vector2 v2 = scm_to_vector2 (s_v2);
    
    scm_remember_upto_here_1 (s_v1);
    scm_remember_upto_here_1 (s_v2);
    
    return scm_from_double (vector2_dot (v1, v2));
}

static SCM
s_vector2_cross (SCM s_v1, SCM s_v2) {
    Vector2 v1 = scm_to_vector2 (s_v1);
    Vector2 v2 = scm_to_vector2 (s_v2);
    
    scm_remember_upto_here_1 (s_v1);
    scm_remember_upto_here_1 (s_v2);
    
    return scm_from_double (vector2_cross (v1, v2));;
}

static SCM
s_vector2_right_normal (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);
    
    scm_remember_upto_here_1 (s_v);

    return scm_from_vector2 (vector2_right_normal (v));
}

static SCM
s_vector2_left_normal (SCM s_v) {
    Vector2 v = scm_to_vector2 (s_v);
    
    scm_remember_upto_here_1 (s_v);

    return scm_from_vector2 (vector2_left_normal (v));
}

void
init_vector2_type (void) {
    vector2_tag = scm_make_smob_type ("<vector2>", sizeof (Vector2));
    scm_set_smob_mark (vector2_tag, 0);
    scm_set_smob_free (vector2_tag, free_vector2);
    scm_set_smob_print (vector2_tag, print_vector2);

    scm_c_define_gsubr ("vector2-eq?", 2, 0, 0, s_vector2_equalp);
    scm_c_define_gsubr ("make-vector2", 2, 0, 0, make_vector2);
    scm_c_define_gsubr ("vector2-from-polar", 2, 0, 0, s_vector2_from_polar);
    scm_c_define_gsubr ("vector2-x", 1, 0, 0, s_vector2_x);
    scm_c_define_gsubr ("vector2-y", 1, 0, 0, s_vector2_y);
    scm_c_define_gsubr ("vector2-add", 0, 0, 1, s_vector2_add);
    scm_c_define_gsubr ("vector2-sub", 1, 0, 1, s_vector2_sub);
    scm_c_define_gsubr ("vector2-scale", 2, 0, 0, s_vector2_scale);
    scm_c_define_gsubr ("vector2-norm", 1, 0, 0, s_vector2_norm);
    scm_c_define_gsubr ("vector2-mag", 1, 0, 0, s_vector2_mag);
    scm_c_define_gsubr ("vector2-angle", 1, 0, 0, s_vector2_angle);
    scm_c_define_gsubr ("vector2-dot", 2, 0, 0, s_vector2_dot);
    scm_c_define_gsubr ("vector2-cross", 2, 0, 0, s_vector2_cross);
    scm_c_define_gsubr ("vector2-left-normal", 1, 0, 0, s_vector2_left_normal);
    scm_c_define_gsubr ("vector2-right-normal", 1, 0, 0, s_vector2_right_normal);

    scm_c_export ("make-vector2", NULL);
    scm_c_export ("vector2-from-polar", NULL);
    scm_c_export ("vector2-x", NULL);
    scm_c_export ("vector2-y", NULL);
    scm_c_export ("vector2-eq?", NULL);
    scm_c_export ("vector2-add", NULL);
    scm_c_export ("vector2-sub", NULL);
    scm_c_export ("vector2-scale", NULL);
    scm_c_export ("vector2-norm", NULL);
    scm_c_export ("vector2-mag", NULL);
    scm_c_export ("vector2-angle", NULL);
    scm_c_export ("vector2-dot", NULL);
    scm_c_export ("vector2-cross", NULL);
    scm_c_export ("vector2-left-normal", NULL);
    scm_c_export ("vector2-right-normal", NULL);
}
