#include "vector.h"

static scm_t_bits vector2_tag;

bool
gmk_vector2_equal (GmkVector2 a, GmkVector2 b)
{
    return a.x == b.x && a.y == b.y;
}

float
gmk_vector2_mag (GmkVector2 v)
{
    return sqrt (v.x * v.x + v.y * v.y);
}

float
gmk_vector2_dot (GmkVector2 a, GmkVector2 b)
{
    return a.x * b.x + a.y * b.y;
}

float
gmk_vector2_cross (GmkVector2 a, GmkVector2 b)
{
    return a.x * b.y - b.x * a.y;
}

float
gmk_vector2_angle (GmkVector2 v)
{
    return atan2 (v.y, v.x);
}

GmkVector2
gmk_vector2_zero ()
{
    GmkVector2 v;

    v.x = 0;
    v.y = 0;

    return v;
}

GmkVector2
gmk_vector2_new (float x, float y)
{
    GmkVector2 v;

    v.x = x;
    v.y = y;

    return v;
}

GmkVector2
gmk_vector2_add (GmkVector2 a, GmkVector2 b)
{
    GmkVector2 c;

    c.x = a.x + b.x;
    c.y = a.y + b.y;

    return c;
}

GmkVector2
gmk_vector2_sub (GmkVector2 a, GmkVector2 b)
{
    GmkVector2 c;

    c.x = a.x - b.x;
    c.y = a.y - b.y;

    return c;
}

GmkVector2
gmk_vector2_scale (GmkVector2 v, float scalar)
{
    GmkVector2 a;

    a.x = v.x * scalar;
    a.y = v.y * scalar;

    return a;
}

GmkVector2
gmk_vector2_norm (GmkVector2 v)
{
    float m = gmk_vector2_mag (v);
    GmkVector2 n = gmk_vector2_zero ();

    if (m != 0) {
        n.x = v.x / m;
        n.y = v.y / m;
    }

    return n;
}

GmkVector2
gmk_vector2_from_polar (float radius, float angle)
{
    GmkVector2 v;

    v.x = cos (angle) * radius;
    v.y = sin (angle) * radius;

    return v;
}

GmkVector2
gmk_vector2_right_normal (GmkVector2 v)
{
    GmkVector2 right;

    right.x = -v.y;
    right.y = v.x;

    return right;
}

GmkVector2
gmk_vector2_left_normal (GmkVector2 v)
{
    GmkVector2 left;

    left.x = v.y;
    left.y = -v.x;

    return left;
}

GmkVector2
gmk_scm_to_vector2 (SCM s_v)
{
    scm_assert_smob_type (vector2_tag, s_v);

    return *((GmkVector2 *) SCM_SMOB_DATA (s_v));
}

SCM
gmk_scm_from_vector2 (GmkVector2 v)
{
    SCM smob;
    GmkVector2 *new_v = (GmkVector2 *) scm_gc_malloc (sizeof (GmkVector2), "vector2");

    *new_v = v;

    SCM_NEWSMOB (smob, vector2_tag, new_v);

    return smob;
}

SCM_DEFINE (gmk_make_vector2, "make-vector2", 2, 0, 0,
            (SCM x, SCM y),
            "Make a 2D vector.")
{
    return gmk_scm_from_vector2 (gmk_vector2_new (scm_to_double (x),
                                                  scm_to_double (y)));
}

SCM_DEFINE (gmk_s_vector2_from_polar, "vector2-from-polar", 2, 0, 0,
            (SCM radius, SCM angle),
            "Make a 2D vector from a polar coordinate pair.")
{
    GmkVector2 v = gmk_vector2_from_polar (scm_to_double (radius),
                                           gmk_deg_to_rad (scm_to_double (angle)));

    return gmk_scm_from_vector2 (v);
}

SCM_DEFINE (gmk_s_vector2_x, "vector2-x", 1, 0, 0,
            (SCM v),
            "Return x coordinate.")
{
    GmkVector2 c_v = gmk_scm_to_vector2 (v);

    return scm_from_double (c_v.x);
}

SCM_DEFINE (gmk_s_vector2_y, "vector2-y", 1, 0, 0,
            (SCM v),
            "Return y coordinate.")
{
    GmkVector2 c_v = gmk_scm_to_vector2 (v);

    return scm_from_double (c_v.y);
}


SCM_DEFINE (gmk_s_vector2_add, "vector2-add", 0, 0, 1,
            (SCM vectors),
            "Sum of vectors.")
{
    GmkVector2 sum = gmk_vector2_zero ();

    while (scm_is_false (scm_null_p (vectors))) {
        SCM s_v = scm_car (vectors);
        GmkVector2 v = gmk_scm_to_vector2 (s_v);

        sum = gmk_vector2_add (sum, v);

        vectors = scm_cdr (vectors);
    }

    return gmk_scm_from_vector2 (sum);
}

SCM_DEFINE (gmk_s_vector2_sub, "vector2-sub", 1, 0, 1,
            (SCM v, SCM vectors),
            "Difference of vectors.")
{
    GmkVector2 difference = gmk_scm_to_vector2 (v);

    while (scm_is_false (scm_null_p (vectors))) {
        SCM s_v = scm_car (vectors);
        GmkVector2 v = gmk_scm_to_vector2 (s_v);

        difference = gmk_vector2_sub (difference, v);

        vectors = scm_cdr (vectors);
    }

    return gmk_scm_from_vector2 (difference);
}

SCM_DEFINE (gmk_s_vector2_scale, "vector2-scale", 2, 0, 0,
            (SCM v, SCM scalar),
            "Multiply vector by a scalar.")
{
    return gmk_scm_from_vector2 (gmk_vector2_scale (gmk_scm_to_vector2 (v),
                                                    scm_to_double (scalar)));
}

SCM_DEFINE (gmk_s_vector2_norm, "vector2-norm", 1, 0, 0,
            (SCM v),
            "Return normalized vector.")
{
    return gmk_scm_from_vector2 (gmk_vector2_norm (gmk_scm_to_vector2 (v)));
}

SCM_DEFINE (gmk_s_vector2_mag, "vector2-mag", 1, 0, 0,
            (SCM v),
            "Return vector magnitude.")
{
    return scm_from_double (gmk_vector2_mag (gmk_scm_to_vector2 (v)));
}

SCM_DEFINE (gmk_s_vector2_angle, "vector2-angle", 1, 0, 0,
            (SCM v),
            "Return vector direction in degrees.")
{
    float theta = gmk_vector2_angle (gmk_scm_to_vector2 (v));

    return scm_from_double (gmk_rad_to_deg (theta));
}

SCM_DEFINE (gmk_s_vector2_dot, "vector2-dot", 2, 0, 0,
            (SCM v1, SCM v2),
            "Return dot product of 2 vectors.")
{
    return scm_from_double (gmk_vector2_dot (gmk_scm_to_vector2 (v1),
                                             gmk_scm_to_vector2 (v2)));
}

SCM_DEFINE (gmk_s_vector2_cross, "vector2-cross", 2, 0, 0,
            (SCM v1, SCM v2),
            "Return cross product of 2 vectors.")
{
    return scm_from_double (gmk_vector2_cross (gmk_scm_to_vector2 (v1),
                                               gmk_scm_to_vector2 (v2)));
}

SCM_DEFINE (gmk_s_vector2_right_normal, "vector2-right-normal", 1, 0, 0,
            (SCM v),
            "Return right normal.")
{
    return gmk_scm_from_vector2 (gmk_vector2_right_normal (gmk_scm_to_vector2 (v)));
}

SCM_DEFINE (gmk_s_vector2_left_normal, "vector2-left-normal", 1, 0, 0,
            (SCM v),
            "Return left normal.")
{
    return gmk_scm_from_vector2 (gmk_vector2_left_normal (gmk_scm_to_vector2 (v)));
}

static size_t
free_vector2 (SCM v)
{
    GmkVector2 *vector = (GmkVector2 *) SCM_SMOB_DATA (v);

    scm_gc_free (vector, sizeof (GmkVector2), "vector2");

    return 0;
}

static int
print_vector2 (SCM v, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<vector2 x: ", port);
    scm_display (gmk_s_vector2_x (v), port);
    scm_puts (" y: ", port);
    scm_display (gmk_s_vector2_y (v), port);
    scm_puts (">", port);

    return 1;
}

static SCM
vector2_equal_p (SCM v1, SCM v2)
{
    return scm_from_bool (gmk_vector2_equal (gmk_scm_to_vector2 (v1),
                                             gmk_scm_to_vector2 (v2)));
}

void
gmk_init_vector2 (void)
{
    vector2_tag = scm_make_smob_type ("vector2", sizeof (GmkVector2));
    scm_set_smob_mark (vector2_tag, 0);
    scm_set_smob_free (vector2_tag, free_vector2);
    scm_set_smob_print (vector2_tag, print_vector2);
    scm_set_smob_equalp (vector2_tag, vector2_equal_p);

#include "vector.x"

    scm_c_export (s_gmk_make_vector2,
                  s_gmk_s_vector2_from_polar,
                  s_gmk_s_vector2_x,
                  s_gmk_s_vector2_y,
                  s_gmk_s_vector2_add,
                  s_gmk_s_vector2_sub,
                  s_gmk_s_vector2_scale,
                  s_gmk_s_vector2_norm,
                  s_gmk_s_vector2_mag,
                  s_gmk_s_vector2_angle,
                  s_gmk_s_vector2_dot,
                  s_gmk_s_vector2_cross,
                  s_gmk_s_vector2_left_normal,
                  s_gmk_s_vector2_right_normal,
                  NULL);
}
