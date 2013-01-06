#ifndef GMK_DRAW_H
#define GMK_DRAW_H

#include "common.h"

SCM gmk_s_clipping_rect (void);
SCM gmk_s_set_clipping_rect (SCM rect);
SCM gmk_s_reset_clipping_rect (void);
SCM gmk_s_draw_line (SCM x1, SCM y1, SCM x2, SCM y2, SCM color, SCM thickness);
void gmk_init_draw (void);

#endif
