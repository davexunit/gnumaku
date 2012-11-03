#ifndef GAME_H
#define GAME_H

#include <time.h>
#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <libguile.h>

typedef struct {
    bool running;
    SCM on_start;
    SCM on_update;
    SCM on_draw;
    SCM on_key_pressed;
    SCM on_key_released;
} Game;

void init_game_type();

#endif
