#ifndef GAME_H
#define GAME_H

#include <time.h>
#include <allegro5/allegro.h>
#include <allegro5/allegro_primitives.h>
#include <allegro5/allegro_image.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>
#include <libguile.h>

typedef struct {
    ALLEGRO_DISPLAY *display;
    ALLEGRO_EVENT_QUEUE *event_queue;
    ALLEGRO_TIMER *timer;
    ALLEGRO_VOICE *voice;
    ALLEGRO_MIXER *mixer;
    char *title;
    float timestep;
    float last_update_time;
    float time_accumulator;
    bool running;
    bool paused;
    bool redraw;
    SCM on_start;
    SCM on_update;
    SCM on_draw;
    SCM on_key_pressed;
    SCM on_key_released;
} Game;

void init_game_type();

#endif
