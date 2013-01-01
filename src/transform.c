#include "transform.h"
#include "math.h"

static scm_t_bits transform_tag;

Transform*
check_transform (SCM transform_smob)
{
    scm_assert_smob_type (transform_tag, transform_smob);

    return (Transform *) SCM_SMOB_DATA (transform_smob);
}

static SCM
make_transform (void)
{
    SCM smob;
    Transform *transform = (Transform *) scm_gc_malloc (sizeof (Transform), "transform");

    SCM_NEWSMOB (smob, transform_tag, transform);
    al_identity_transform (&transform->transform);

    return smob;
}

static SCM
get_current_transform (void)
{
    SCM smob;
    Transform *transform = (Transform *) scm_gc_malloc (sizeof (Transform), "transform");

    al_copy_transform (&transform->transform, al_get_current_transform ());
    SCM_NEWSMOB (smob, transform_tag, transform);

    return smob;
}

static SCM
copy_transform (SCM transform_smob_dest, SCM transform_smob_src)
{
    Transform *transform_dest = check_transform (transform_smob_dest);
    Transform *transform_src = check_transform (transform_smob_src);

    al_copy_transform (&transform_dest->transform, &transform_src->transform);

    return SCM_UNSPECIFIED;
}

static SCM
compose_transform (SCM transform_smob_dest, SCM transform_smob_src)
{
    Transform *transform_dest = check_transform (transform_smob_dest);
    Transform *transform_src = check_transform (transform_smob_src);

    al_compose_transform (&transform_dest->transform, &transform_src->transform);

    return SCM_UNSPECIFIED;
}

static SCM
use_transform (SCM transform_smob)
{
    Transform *transform = check_transform (transform_smob);

    al_use_transform (&transform->transform);

    return SCM_UNSPECIFIED;
}

static SCM
invert_transform (SCM transform_smob)
{
    Transform *transform = check_transform (transform_smob);

    al_invert_transform (&transform->transform);

    return SCM_UNSPECIFIED;
}

static SCM
identity_transform (SCM transform_smob)
{
    Transform *transform = check_transform (transform_smob);

    al_identity_transform (&transform->transform);

    return SCM_UNSPECIFIED;
}

static SCM
build_transform (SCM transform_smob, SCM s_x, SCM s_y, SCM s_scale_x, SCM s_scale_y, SCM s_rotation)
{
    Transform *transform = check_transform (transform_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    float scale_x = scm_to_double (s_scale_x);
    float scale_y = scm_to_double (s_scale_y);
    float theta = deg2rad (scm_to_double (s_rotation));

    al_build_transform (&transform->transform, x, y, scale_x, scale_y, theta);

    return SCM_UNSPECIFIED;
}

static SCM
translate_transform (SCM transform_smob, SCM s_x, SCM s_y)
{
    Transform *transform = check_transform (transform_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    al_translate_transform (&transform->transform, x, y);

    return SCM_UNSPECIFIED;
}

static SCM
rotate_transform (SCM transform_smob, SCM s_rotation)
{
    Transform *transform = check_transform (transform_smob);
    float theta = deg2rad (scm_to_double (s_rotation));

    al_rotate_transform (&transform->transform, theta);

    return SCM_UNSPECIFIED;
}

static SCM
scale_transform (SCM transform_smob, SCM s_scale_x, SCM s_scale_y)
{
    Transform *transform = check_transform (transform_smob);
    float scale_x = scm_to_double (s_scale_x);
    float scale_y = scm_to_double (s_scale_y);

    al_scale_transform (&transform->transform, scale_x, scale_y);

    return SCM_UNSPECIFIED;
}

static SCM
transform_coordinates (SCM transform_smob, SCM s_x, SCM s_y)
{
    Transform *transform = check_transform (transform_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    al_transform_coordinates (&transform->transform, &x, &y);

    return scm_list_2 (scm_from_double (x), scm_from_double (y));
}

static size_t
free_transform (SCM transform_smob)
{
    Transform *transform = (Transform *) SCM_SMOB_DATA (transform_smob);

    scm_gc_free (transform, sizeof (Transform), "transform");

    return 0;
}

static int
print_transform (SCM transform_smob, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<transform >", port);

    /* non-zero means success */
    return 1;
}

void
init_transform_type (void)
{
    transform_tag = scm_make_smob_type ("Transform", sizeof (Transform));
    scm_set_smob_mark (transform_tag, 0);
    scm_set_smob_free (transform_tag, free_transform);
    scm_set_smob_print (transform_tag, print_transform);

    scm_c_define_gsubr ("make-transform", 1, 0, 0, make_transform);
    scm_c_define_gsubr ("current-transform", 0, 0, 0, get_current_transform);
    scm_c_define_gsubr ("copy-transform!", 2, 0, 0, copy_transform);
    scm_c_define_gsubr ("compose-transform!", 2, 0, 0, compose_transform);
    scm_c_define_gsubr ("use-transform", 1, 0, 0, use_transform);
    scm_c_define_gsubr ("invert-transform!", 1, 0, 0, invert_transform);
    scm_c_define_gsubr ("identity-transform!", 1, 0, 0, identity_transform);
    scm_c_define_gsubr ("build-transform!", 6, 0, 0, build_transform);
    scm_c_define_gsubr ("translate-transform!", 3, 0, 0, translate_transform);
    scm_c_define_gsubr ("rotate-transform!", 2, 0, 0, rotate_transform);
    scm_c_define_gsubr ("scale-transform!", 3, 0, 0, scale_transform);
    scm_c_define_gsubr ("transform-coordinates", 3, 0, 0, transform_coordinates);

    scm_c_export ("make-transform", NULL);
    scm_c_export ("current-transform", NULL);
    scm_c_export ("copy-transform!", NULL);
    scm_c_export ("compose-transform!", NULL);
    scm_c_export ("use-transform", NULL);
    scm_c_export ("invert-transform!", NULL);
    scm_c_export ("identity-transform!", NULL);
    scm_c_export ("build-transform!", NULL);
    scm_c_export ("translate-transform!", NULL);
    scm_c_export ("rotate-transform!", NULL);
    scm_c_export ("scale-transform!", NULL);
    scm_c_export ("transform-coordinates", NULL);
}
