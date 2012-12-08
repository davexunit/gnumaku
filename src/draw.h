#ifndef DRAW_H
#define DRAW_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_primitives.h>
#include <libguile.h>

ALLEGRO_COLOR scm_to_color (SCM s_color_list);
void bind_draw_funcs ();

#endif
