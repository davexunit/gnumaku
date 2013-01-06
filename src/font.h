#ifndef GMK_FONT_H
#define GMK_FONT_H

#include "common.h"

/*
 * GmkFont
 *
 * A wrapper around ALLEGRO_FONT since allegro does it's own thing with
 * memory allocation.
 */
typedef struct {
    ALLEGRO_FONT *font;
} GmkFont;

SCM gmk_load_font (SCM filename, SCM size);
SCM gmk_draw_text (SCM font, SCM position, SCM color, SCM text);
void gmk_init_font (void);

#endif
