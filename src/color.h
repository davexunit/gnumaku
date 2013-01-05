#ifndef GMK_COLOR_H
#define GMK_COLOR_H

#include "common.h"

ALLEGRO_COLOR gmk_color_mult_alpha (ALLEGRO_COLOR color);
ALLEGRO_COLOR gmk_scm_to_color (SCM s_color);
SCM gmk_scm_from_color (ALLEGRO_COLOR color);
SCM gmk_s_make_color (SCM r, SCM g, SCM b, SCM a);
SCM gmk_s_make_color_f (SCM r, SCM g, SCM b, SCM a);
SCM gmk_s_color_r (SCM color);
SCM gmk_s_color_g (SCM color);
SCM gmk_s_color_b (SCM color);
SCM gmk_s_color_a (SCM color);
void gmk_init_color (void);

#endif
