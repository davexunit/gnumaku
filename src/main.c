#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>

#include "sprite_sheet.h"
#include "sprite.h"
#include "font.h"
#include "game.h"
#include "bullet_system.h"

void
init_gnumaku ()
{
    // Create smobs so we can call C code from Scheme
    init_sprite_sheet_type ();
    init_sprite_type ();
    init_rect_type ();
    init_font_type ();
    init_game_type ();
    init_bullet_system_type ();
}

void
init_gnumaku_module ()
{
    // Create the gnumaku core module and export all procedures
    scm_c_define_module ("gnumaku core", init_gnumaku, NULL);
}
