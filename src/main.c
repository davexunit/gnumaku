#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>

#include "transform.h"
#include "sprite_sheet.h"
#include "sprite.h"
#include "font.h"
#include "audio.h"
#include "game.h"
#include "bullet_system.h"
#include "draw.h"

void
init_gnumaku ()
{
    // Create smobs so we can call C code from Scheme
    init_transform_type ();
    init_sprite_sheet_type ();
    init_image_type ();
    init_sprite_type ();
    init_rect_type ();
    init_font_type ();
    init_sample_type ();
    init_game_type ();
    init_bullet_system_type ();
    bind_draw_funcs ();
}

void
init_gnumaku_module ()
{
    // Create the gnumaku core module and export all procedures
    scm_c_define_module ("gnumaku core", init_gnumaku, NULL);
}
