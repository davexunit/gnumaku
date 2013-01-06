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
gmk_init (void)
{
    gmk_init_vector2 ();
    gmk_init_transform ();
    gmk_init_color ();
    gmk_init_blend_mode ();
    gmk_init_sprite_sheet ();
    gmk_init_image ();
    gmk_init_sprite ();
    gmk_init_rect ();
    gmk_init_font ();
    gmk_init_audio ();
    gmk_init_particle_system ();
    gmk_init_bullet_type ();
    init_bullet_system_type ();
    gmk_init_draw ();
    gmk_init_game ();
}
