#include "sprite_sheet.h"
#include "image.h"

static scm_t_bits sprite_sheet_tag;

ALLEGRO_BITMAP *
gmk_sprite_sheet_tile (GmkSpriteSheet *sprite_sheet, int index)
{
    return sprite_sheet->tiles[index];
}

GmkSpriteSheet *
gmk_scm_to_sprite_sheet (SCM sprite_sheet)
{
    scm_assert_smob_type (sprite_sheet_tag, sprite_sheet);

    return (GmkSpriteSheet *) SCM_SMOB_DATA (sprite_sheet);
}

static void
init_sprite_sheet_tiles (GmkSpriteSheet *sprite_sheet)
{
    int image_width = al_get_bitmap_width (sprite_sheet->image);
    int image_height = al_get_bitmap_height (sprite_sheet->image);
    int tile_width = sprite_sheet->tile_width;
    int tile_height = sprite_sheet->tile_height;
    int spacing = sprite_sheet->spacing;
    int margin = sprite_sheet->margin;
    int columns = (image_width - margin) / (tile_width + spacing);
    int rows = (image_height - margin) / (tile_height + spacing);

    sprite_sheet->num_tiles = rows * columns;
    sprite_sheet->tiles = (ALLEGRO_BITMAP ** )scm_gc_malloc (sizeof (ALLEGRO_BITMAP **)
                                                             * sprite_sheet->num_tiles,
                                                             "tiles");

    for (int y = 0; y < rows; ++y) {
        for (int x = 0; x < columns; ++x) {
            int index = y * columns + x;
            int tile_x = margin + x * (tile_width + spacing);
            int tile_y = margin + y * (tile_height + spacing);

            ALLEGRO_BITMAP *tile = al_create_sub_bitmap (sprite_sheet->image,
                                                         tile_x, tile_y,
                                                         tile_width, tile_height);
            sprite_sheet->tiles[index] = tile;
        }
    }
}

SCM_DEFINE (gmk_s_load_sprite_sheet, "load-sprite-sheet", 5, 0, 0,
            (SCM filename, SCM tile_width, SCM tile_height, SCM spacing, SCM margin),
            "Load sprite sheet from an image file.")
{
    SCM smob;
    GmkSpriteSheet *sprite_sheet;

    sprite_sheet = (GmkSpriteSheet *) scm_gc_malloc (sizeof (GmkSpriteSheet),
                                                     "sprite_sheet");

    sprite_sheet->image = NULL;
    sprite_sheet->tile_width = scm_to_int (tile_width);
    sprite_sheet->tile_height = scm_to_int (tile_height);
    sprite_sheet->spacing = scm_to_int (spacing);
    sprite_sheet->margin = scm_to_int (margin);

    SCM_NEWSMOB (smob, sprite_sheet_tag, sprite_sheet);

    sprite_sheet->image = al_load_bitmap (scm_to_locale_string (filename));
    init_sprite_sheet_tiles (sprite_sheet);

    return smob;
}

SCM_DEFINE (gmk_s_sprite_sheet_tile_width, "sprite-sheet-tile-width", 1, 0, 0,
            (SCM sprite_sheet),
            "Return tile width.")
{
    GmkSpriteSheet *ss = gmk_scm_to_sprite_sheet (sprite_sheet);

    return scm_from_int (ss->tile_width);
}

SCM_DEFINE (gmk_s_sprite_sheet_tile_height, "sprite-sheet-tile-width", 1, 0, 0,
            (SCM sprite_sheet),
            "Return tile width.")
{
    GmkSpriteSheet *ss = gmk_scm_to_sprite_sheet (sprite_sheet);

    return scm_from_int (ss->tile_height);
}

SCM_DEFINE (gmk_s_sprite_sheet_spacing, "sprite-sheet-tile-width", 1, 0, 0,
            (SCM sprite_sheet),
            "Return tile width.")
{
    GmkSpriteSheet *ss = gmk_scm_to_sprite_sheet (sprite_sheet);

    return scm_from_int (ss->spacing);
}

SCM_DEFINE (gmk_s_sprite_sheet_margin, "sprite-sheet-tile-width", 1, 0, 0,
            (SCM sprite_sheet),
            "Return tile width.")
{
    GmkSpriteSheet *ss = gmk_scm_to_sprite_sheet (sprite_sheet);

    return scm_from_int (ss->margin);
}

SCM_DEFINE (gmk_s_sprite_sheet_tile, "sprite-sheet-tile", 2, 0, 0,
            (SCM sprite_sheet, SCM index),
            "Return tile image at @var{index}.")
{
    GmkSpriteSheet *ss = gmk_scm_to_sprite_sheet (sprite_sheet);

    return gmk_scm_from_bitmap (gmk_sprite_sheet_tile (ss, scm_to_int (index)));
}

static size_t
free_sprite_sheet (SCM sprite_sheet)
{
    GmkSpriteSheet *ss = (GmkSpriteSheet *) SCM_SMOB_DATA (sprite_sheet);

    /* Destroy all sub-bitmaps. */
    for (int i = 0; i < ss->num_tiles; ++i) {
        al_destroy_bitmap (ss->tiles[i]);
    }

    al_destroy_bitmap (ss->image);

    scm_gc_free (ss->tiles,
                 sizeof (ALLEGRO_BITMAP **) * ss->num_tiles,
                 "tiles");
    scm_gc_free (ss, sizeof (GmkSpriteSheet), "sprite_sheet");

    return 0;
}

static int
print_sprite_sheet (SCM sprite_sheet, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<sprite-sheet tile-width: ", port);
    scm_display (gmk_s_sprite_sheet_tile_width (sprite_sheet), port);
    scm_puts (" tile-height: ", port);
    scm_display (gmk_s_sprite_sheet_tile_height (sprite_sheet), port);
    scm_puts (" spacing: ", port);
    scm_display (gmk_s_sprite_sheet_spacing (sprite_sheet), port);
    scm_puts (" margin: ", port);
    scm_display (gmk_s_sprite_sheet_margin (sprite_sheet), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_sprite_sheet (void)
{
    sprite_sheet_tag = scm_make_smob_type ("GmkSpriteSheet", sizeof(GmkSpriteSheet));
    scm_set_smob_mark (sprite_sheet_tag, 0);
    scm_set_smob_free (sprite_sheet_tag, free_sprite_sheet);
    scm_set_smob_print (sprite_sheet_tag, print_sprite_sheet);

#include "sprite_sheet.x"

    scm_c_export (s_gmk_s_load_sprite_sheet,
                  s_gmk_s_sprite_sheet_tile_width,
                  s_gmk_s_sprite_sheet_tile_height,
                  s_gmk_s_sprite_sheet_spacing,
                  s_gmk_s_sprite_sheet_margin,
                  s_gmk_s_sprite_sheet_tile,
                  NULL);
}
