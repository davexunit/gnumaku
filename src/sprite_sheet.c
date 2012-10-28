#include "sprite_sheet.h"

static scm_t_bits sprite_sheet_tag;

SpriteSheet*
check_sprite_sheet (SCM sprite_sheet_smob)
{
    scm_assert_smob_type (sprite_sheet_tag, sprite_sheet_smob);

    return (SpriteSheet *) SCM_SMOB_DATA (sprite_sheet_smob);
}

static void
init_sprite_sheet_tiles (SpriteSheet *sprite_sheet)
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
    sprite_sheet->tiles = (ALLEGRO_BITMAP **) scm_gc_malloc (sizeof (ALLEGRO_BITMAP **) * sprite_sheet->num_tiles, "tiles");
    
    for (int y = 0; y < rows; ++y)
    {
	for (int x = 0; x < columns; ++x)
	{
	    int index = y * columns + x;
	    int tile_x = margin + x * (tile_width + spacing);
	    int tile_y = margin + y * (tile_height + spacing);

	    ALLEGRO_BITMAP *tile = al_create_sub_bitmap (sprite_sheet->image, tile_x, tile_y, tile_width, tile_height);
	    sprite_sheet->tiles[index] = tile;
	}
    }
}

static SCM
make_sprite_sheet (SCM s_file, SCM s_tile_width, SCM s_tile_height, SCM s_spacing, SCM s_margin)
{
    SCM smob;
    SpriteSheet *sprite_sheet;
    const char *file = scm_to_locale_string (s_file);
    int tile_width = scm_to_int (s_tile_width);
    int tile_height = scm_to_int (s_tile_height);
    int spacing = scm_to_int (s_spacing);
    int margin = scm_to_int (s_margin);

    sprite_sheet = (SpriteSheet *) scm_gc_malloc (sizeof (SpriteSheet), "sprite_sheet");

    sprite_sheet->image = NULL;
    sprite_sheet->tile_width = tile_width;
    sprite_sheet->tile_height = tile_height;
    sprite_sheet->spacing = spacing;
    sprite_sheet->margin = margin;

    SCM_NEWSMOB (smob, sprite_sheet_tag, sprite_sheet);

    sprite_sheet->image = al_load_bitmap (file);
    init_sprite_sheet_tiles (sprite_sheet);

    return smob;
}

static size_t
free_sprite_sheet (SCM sprite_sheet_smob)
{
    SpriteSheet *sprite_sheet = (SpriteSheet *) SCM_SMOB_DATA (sprite_sheet_smob);

    al_destroy_bitmap(sprite_sheet->image);
    
    for (int i = 0; i < sprite_sheet->num_tiles; ++i)
    {
	al_destroy_bitmap (sprite_sheet->tiles + i);
    }

    scm_gc_free (sprite_sheet->tiles, sizeof (ALLEGRO_BITMAP **) * sprite_sheet->num_tiles, "tiles");
    scm_gc_free (sprite_sheet, sizeof (SpriteSheet), "sprite_sheet");

    return 0;
}

static int
print_sprite_sheet (SCM sprite_sheet_smob, SCM port, scm_print_state *pstate)
{
    SpriteSheet *sprite_sheet = (SpriteSheet *) SCM_SMOB_DATA (sprite_sheet_smob);
     
    scm_puts ("#<SpriteSheet ", port);
    scm_display (scm_from_int(sprite_sheet->tile_width), port);
    scm_puts (" ", port);
    scm_display (scm_from_int(sprite_sheet->tile_height), port);
    scm_puts (" ", port);
    scm_display (scm_from_int(sprite_sheet->spacing), port);
    scm_puts (" ", port);
    scm_display (scm_from_int(sprite_sheet->margin), port);
    scm_puts (">", port);
     
    /* non-zero means success */
    return 1;
}

void
init_sprite_sheet_type (void)
{
    sprite_sheet_tag = scm_make_smob_type ("SpriteSheet", sizeof(SpriteSheet));
    scm_set_smob_mark (sprite_sheet_tag, 0);
    scm_set_smob_free (sprite_sheet_tag, free_sprite_sheet);
    scm_set_smob_print (sprite_sheet_tag, print_sprite_sheet);

    scm_c_define_gsubr ("make-sprite-sheet", 5, 0, 0, make_sprite_sheet);
}

void
draw_sprite_sheet_tile (SpriteSheet *sprite_sheet, int tile, float x, float y)
{
    if (sprite_sheet->image)
    {
	al_draw_bitmap(sprite_sheet->tiles[tile], x, y, 0);
    }
}
