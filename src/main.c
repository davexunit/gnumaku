#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>

#include "math.h"
#include "sprite_sheet.h"
#include "sprite.h"
#include "game.h"
#include "bullet_system.h"

static void inner_main();

int
main (int argc, char **argv) {
    scm_boot_guile(argc, argv, inner_main, 0);

    return 0;
}

static void
inner_main (void *closure, int argc, char **argv) {
    init_sprite_sheet_type ();
    init_sprite_type ();
    init_rect_type ();
    init_game_type ();
    init_bullet_system_type ();
    scm_c_primitive_load ("scripts/main.scm");
}
