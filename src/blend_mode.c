#include "blend_mode.h"

SCM_SYMBOL (sym_blend_alpha, "alpha");
SCM_SYMBOL (sym_blend_add, "add");

GmkBlendMode
gmk_scm_to_blend_mode (SCM s_blend_mode)
{
    GmkBlendMode blend_mode;
    
    if (scm_is_true (scm_eq_p (s_blend_mode, sym_blend_alpha))) {
        blend_mode = GMK_BLEND_ALPHA;
    } else if (scm_is_true (scm_eq_p (s_blend_mode, sym_blend_add))) {
        blend_mode = GMK_BLEND_ADD;
    } else {
        scm_error_scm (scm_from_latin1_symbol ("blend-mode-error"),
                       SCM_BOOL_F,
                       scm_from_locale_string ("Unrecognized blend mode"),
                       SCM_EOL, SCM_BOOL_F);
    }

    return blend_mode;
}

void
gmk_set_blend_mode (GmkBlendMode blend_mode)
{
    switch (blend_mode) {
    case GMK_BLEND_ALPHA:
        al_set_blender(ALLEGRO_ADD, ALLEGRO_ONE, ALLEGRO_INVERSE_ALPHA);
        break;
    case GMK_BLEND_ADD:
        al_set_blender(ALLEGRO_ADD, ALLEGRO_ALPHA, ALLEGRO_ONE);
        break;
    }
}

void
init_blend_mode (void)
{
#include "blend_mode.x"
}
