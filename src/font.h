#ifndef FONT_H
#define FONT_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_font.h>
#include <allegro5/allegro_ttf.h>
#include <libguile.h>

typedef struct
{
    ALLEGRO_FONT *font;
} Font;

void init_font_type (void);

#endif
