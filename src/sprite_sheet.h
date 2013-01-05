#ifndef GMK_SPRITE_SHEET_H
#define GMK_SPRITE_SHEET_H

#include "common.h"

/*
 * GmkSpriteSheet
 *
 * Splits an image into a grid of many small tiles.
 */
typedef struct {
    ALLEGRO_BITMAP *image;
    int tile_width;
    int tile_height;
    int spacing;
    int margin;
    int num_tiles;
    ALLEGRO_BITMAP **tiles;
} GmkSpriteSheet;

ALLEGRO_BITMAP *gmk_sprite_sheet_tile (GmkSpriteSheet *sprite_sheet, int index);
GmkSpriteSheet *gmk_scm_to_sprite_sheet (SCM sprite_sheet);
SCM gmk_s_load_sprite_sheet (SCM filename, SCM tile_width, SCM tile_height,
                           SCM spacing, SCM margin);
SCM gmk_s_sprite_sheet_tile_width (SCM sprite_sheet);
SCM gmk_s_sprite_sheet_tile_height (SCM sprite_sheet);
SCM gmk_s_sprite_sheet_spacing (SCM sprite_sheet);
SCM gmk_s_sprite_sheet_margin (SCM sprite_sheet);
SCM gmk_s_sprite_sheet_tile (SCM sprite_sheet, SCM index);
void gmk_init_sprite_sheet (void);

#endif
