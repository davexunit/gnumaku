#ifndef GNUMAKU_COLOR_H
#define GNUMAKU_COLOR_H

#include <allegro5/allegro.h>
#include <libguile.h>

typedef struct {
    ALLEGRO_COLOR color;
} Color;

SCM scm_from_color (ALLEGRO_COLOR color);
ALLEGRO_COLOR scm_to_color (SCM s_color);
void init_color_type (void);

#endif
