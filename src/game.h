#ifndef GAME_H
#define GAME_H

#include <time.h>
#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <libguile.h>

typedef struct {
    SCM on_start;
    SCM on_update;
    SCM on_draw;
} Game;

void init_game_type();

#endif
