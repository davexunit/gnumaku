#include <stdlib.h>
#include <stdio.h>
#include <libguile.h>

#include "vector.h"
#include "transform.h"
#include "sprite_sheet.h"
#include "sprite.h"
#include "font.h"
#include "audio.h"
#include "game.h"
#include "particle_system.h"
#include "bullet_type.h"
#include "bullet_system.h"
#include "draw.h"

void
init_gnumaku () {
    /* Create smobs so we can call C code from Scheme. */
    gmk_init_vector2 ();
    init_transform_type ();
    init_color ();
    init_blend_mode ();
    init_sprite_sheet_type ();
    gmk_init_image ();
    gmk_init_sprite ();
    init_rect ();
    init_font_type ();
    init_sample_type ();
    init_audio_stream_type ();
    init_particle_system_type ();
    gmk_init_bullet_type ();
    init_bullet_system_type ();
    bind_draw_funcs ();
    init_game_bindings ();
}
