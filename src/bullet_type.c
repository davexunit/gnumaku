#include "bullet_type.h"

static scm_t_bits bullet_type_tag;
static SCM sym_blend_alpha;
static SCM sym_blend_add;

BlendMode
scm_to_blend_mode (SCM blend_mode) {
    if (scm_is_true (scm_eq_p (blend_mode, sym_blend_alpha))) {
        return BLEND_ALPHA;
    } else if (scm_is_true (scm_eq_p (blend_mode, sym_blend_add))) {
        return BLEND_ADD;
    }

    /* Standard alpha blending by default. */
    return BLEND_ALPHA;
}

BulletType*
check_bullet_type (SCM bullet_type_smob) {
    scm_assert_smob_type (bullet_type_tag, bullet_type_smob);

    return (BulletType *) SCM_SMOB_DATA (bullet_type_smob);
}

static SCM
make_bullet_type (SCM s_image, SCM s_hitbox, SCM s_blend_mode, SCM s_directional) {
    SCM smob;
    BulletType *bullet_type;
    int image = scm_to_int (s_image);
    Rect hitbox = scm_to_rect (s_hitbox);
    BlendMode blend_mode = scm_to_blend_mode (s_blend_mode);
    bool directional = scm_to_bool (s_directional);

    bullet_type = (BulletType *) scm_gc_malloc (sizeof (BulletType), "bullet_type");
    bullet_type->image = image;
    bullet_type->hitbox = hitbox;
    bullet_type->blend_mode = blend_mode;
    bullet_type->directional = directional;
    SCM_NEWSMOB (smob, bullet_type_tag, bullet_type);

    return smob;
}

static size_t
free_bullet_type (SCM bullet_type_smob) {
    BulletType *bullet_type = (BulletType *) SCM_SMOB_DATA (bullet_type_smob);

    scm_gc_free (bullet_type, sizeof (BulletType), "bullet_type");

    return 0;
}

static int
print_bullet_type (SCM bullet_type_smob, SCM port, scm_print_state *pstate) {
    scm_puts ("#<bullet-type>", port);

    return 1;
}

void
init_bullet_type_type (void) {
    sym_blend_alpha = scm_from_latin1_symbol ("alpha");
    sym_blend_add = scm_from_latin1_symbol ("add");

    bullet_type_tag = scm_make_smob_type ("<bullet-type>", sizeof (BulletType));
    scm_set_smob_mark (bullet_type_tag, 0);
    scm_set_smob_free (bullet_type_tag, free_bullet_type);
    scm_set_smob_print (bullet_type_tag, print_bullet_type);

    scm_c_define_gsubr ("make-bullet-type", 4, 0, 0, make_bullet_type);

    scm_c_export ("make-bullet-type", NULL);
}
