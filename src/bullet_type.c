#include "bullet_type.h"

static scm_t_bits bullet_type_tag;

GmkBulletType *
gmk_check_bullet_type (SCM bullet_type_smob)
{
    scm_assert_smob_type (bullet_type_tag, bullet_type_smob);

    return (GmkBulletType *) SCM_SMOB_DATA (bullet_type_smob);
}

SCM_DEFINE (gmk_make_bullet_type, "make-bullet-type", 4, 0, 0,
            (SCM s_image, SCM s_hitbox, SCM s_blend_mode, SCM s_directional),
            "Make a new bullet type instance.")
{
    GmkBulletType *bullet_type;
    int image = scm_to_int (s_image);
    Rect hitbox = scm_to_rect (s_hitbox);
    GmkBlendMode blend_mode = gmk_scm_to_blend_mode (s_blend_mode);
    bool directional = scm_to_bool (s_directional);

    bullet_type = (GmkBulletType *) scm_gc_malloc (sizeof (GmkBulletType),
                                                   "bullet_type");
    bullet_type->image = image;
    bullet_type->hitbox = hitbox;
    bullet_type->blend_mode = blend_mode;
    bullet_type->directional = directional;

    SCM_RETURN_NEWSMOB (bullet_type_tag, bullet_type);
}

static size_t
free_bullet_type (SCM bullet_type_smob)
{
    GmkBulletType *bullet_type = (GmkBulletType *) SCM_SMOB_DATA (bullet_type_smob);

    scm_gc_free (bullet_type, sizeof (GmkBulletType), "bullet_type");

    return 0;
}

static int
print_bullet_type (SCM bullet_type_smob, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<bullet-type>", port);

    return 1;
}

void
gmk_init_bullet_type (void)
{
    bullet_type_tag = scm_make_smob_type ("<bullet-type>", sizeof (GmkBulletType));
    scm_set_smob_mark (bullet_type_tag, 0);
    scm_set_smob_free (bullet_type_tag, free_bullet_type);
    scm_set_smob_print (bullet_type_tag, print_bullet_type);

#include "bullet_type.x"

    scm_c_export (s_gmk_make_bullet_type, NULL);
}
