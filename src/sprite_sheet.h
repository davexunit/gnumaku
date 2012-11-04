#ifndef SPRITE_SHEET_H
#define SPRITE_SHEET_H

#include <libguile.h>
#include <allegro5/allegro.h>
#include <allegro5/allegro_image.h>

typedef struct
{
    ALLEGRO_BITMAP *image;
    int tile_width;
    int tile_height;
    int spacing;
    int margin;
    int num_tiles;
    ALLEGRO_BITMAP **tiles;
} SpriteSheet;

SpriteSheet* check_sprite_sheet (SCM sprite_sheet_smob);
void init_sprite_sheet_type (void);

#endif
