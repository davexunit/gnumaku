#include "transform.h"

static scm_t_bits transform_tag;

static ALLEGRO_TRANSFORM *
check_transform (SCM transform)
{
    scm_assert_smob_type (transform_tag, transform);

    return (ALLEGRO_TRANSFORM *) SCM_SMOB_DATA (transform);
}

static ALLEGRO_TRANSFORM *
malloc_transform (void)
{
    return (ALLEGRO_TRANSFORM *) scm_gc_malloc (sizeof (ALLEGRO_TRANSFORM), "transform");
}

SCM_DEFINE (gmk_s_make_transform, "make-transform", 5, 0, 0,
            (SCM x, SCM y, SCM scale_x, SCM scale_y, SCM rotation),
            "Make transformation matrix.")
{
    ALLEGRO_TRANSFORM *transform = malloc_transform ();

    al_build_transform (transform, scm_to_double (x), scm_to_double (y),
                        scm_to_double (scale_x), scm_to_double (scale_y),
                        gmk_deg_to_rad (scm_to_double (rotation)));
    SCM_RETURN_NEWSMOB (transform_tag, transform);
}

SCM_DEFINE (gmk_s_make_identity_transform, "make-identity-transform", 0, 0, 0,
            (void),
            "Make identity transformation matrix.")
{
    ALLEGRO_TRANSFORM *transform = malloc_transform ();

    al_identity_transform (transform);
    SCM_RETURN_NEWSMOB (transform_tag, transform);
}

SCM_DEFINE (gmk_s_current_transform, "current-transform", 0, 0, 0,
            (void),
            "Return currently applied transformation matrix.")
{
    ALLEGRO_TRANSFORM *transform = malloc_transform ();

    al_copy_transform (transform, al_get_current_transform ());
    SCM_RETURN_NEWSMOB (transform_tag, transform);
}

SCM_DEFINE (gmk_s_copy_transform, "copy-transform", 2, 0, 0,
            (SCM transform_dest, SCM transform_src),
            "Copies @var{transform_src} into @var{transform_dest}.")
{
    ALLEGRO_TRANSFORM *t_dest = check_transform (transform_dest);
    ALLEGRO_TRANSFORM *t_src = check_transform (transform_src);

    al_copy_transform (t_dest, t_src);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_compose_transform, "compose-transform", 2, 0, 0,
            (SCM transform_dest, SCM transform_src),
            "Compose transformation.")
{
    ALLEGRO_TRANSFORM *t_dest = check_transform (transform_dest);
    ALLEGRO_TRANSFORM *t_src = check_transform (transform_src);

    al_compose_transform (t_dest, t_src);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_use_transform, "use-transform", 1, 0, 0,
            (SCM transform),
            "Set @var{transform} to be the current global transformation matrix.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_use_transform (t);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_invert_transform, "invert-transform", 1, 0, 0,
            (SCM transform),
            "Invert transformation matrix.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_invert_transform (t);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_identity_transform, "identify-transform", 1, 0, 0,
            (SCM transform),
            "Set @var{transform} to be the identity matrix.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_identity_transform (t);

    return SCM_UNSPECIFIED;
}

/* SCM_DEFINE (gmk_s_build_transform, "build-transform" (SCM transform, SCM x, SCM y, SCM scale_x, SCM scale_y, SCM s_rotation) */
/* { */
/*     ALLEGRO_TRANSFORM *t = check_transform (transform); */
/*     float x = scm_to_double (s_x); */
/*     float y = scm_to_double (s_y); */
/*     float scale_x = scm_to_double (s_scale_x); */
/*     float scale_y = scm_to_double (s_scale_y); */
/*     float theta = gmk_deg_to_rad (scm_to_double (s_rotation)); */

/*     al_build_transform (transform, x, y, scale_x, scale_y, theta); */

/*     return SCM_UNSPECIFIED; */
/* } */

SCM_DEFINE (gmk_s_translate_transform, "translate-transform", 3, 0, 0,
            (SCM transform, SCM x, SCM y),
            "Translate matrix.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_translate_transform (t,
                            scm_to_double (x),
                            scm_to_double (y));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_rotate_transform, "rotate-transform", 2, 0, 0,
            (SCM transform, SCM rotation),
            "Rotate matrix.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_rotate_transform (t,
                         gmk_deg_to_rad (scm_to_double (rotation)));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_scale_transform, "scale-transform", 3, 0, 0,
            (SCM transform, SCM scale_x, SCM scale_y),
            "Scale matrix")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);

    al_scale_transform (t,
                        scm_to_double (scale_x),
                        scm_to_double (scale_y));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_transform_coordinates, "transform-coordinates", 3, 0, 0,
            (SCM transform, SCM v),
            "Return @var{v} multiplied by @var{transform}.")
{
    ALLEGRO_TRANSFORM *t = check_transform (transform);
    GmkVector2 vector = gmk_scm_to_vector2 (v);

    al_transform_coordinates (t, &vector.x, &vector.y);

    return gmk_scm_from_vector2 (vector);
}

static size_t
free_transform (SCM transform)
{
    ALLEGRO_TRANSFORM *t = (ALLEGRO_TRANSFORM *) SCM_SMOB_DATA (transform);

    scm_gc_free (t, sizeof (ALLEGRO_TRANSFORM), "transform");

    return 0;
}

static int
print_transform (SCM transform, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<transform >", port);

    /* non-zero means success */
    return 1;
}

void
gmk_init_transform (void)
{
    transform_tag = scm_make_smob_type ("transform", sizeof (ALLEGRO_TRANSFORM));
    scm_set_smob_mark (transform_tag, 0);
    scm_set_smob_free (transform_tag, free_transform);
    scm_set_smob_print (transform_tag, print_transform);

#include "transform.x"

    scm_c_export (s_gmk_s_make_transform,
                  s_gmk_s_make_identity_transform,
                  s_gmk_s_current_transform,
                  s_gmk_s_copy_transform,
                  s_gmk_s_compose_transform,
                  s_gmk_s_use_transform,
                  s_gmk_s_invert_transform,
                  s_gmk_s_identity_transform,
                  s_gmk_s_translate_transform,
                  s_gmk_s_rotate_transform,
                  s_gmk_s_scale_transform,
                  s_gmk_s_transform_coordinates,
                  NULL);
}
