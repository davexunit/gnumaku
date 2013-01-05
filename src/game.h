#ifndef GMK_GAME_H
#define GMK_GAME_H

#include "common.h"

SCM gmk_s_game_on_start_hook (SCM callback);
SCM gmk_s_game_on_update_hook (SCM callback);
SCM gmk_s_game_on_draw_hook (SCM callback);
SCM gmk_s_game_on_key_pressed_hook (SCM callback);
SCM gmk_s_game_on_key_released_hook (SCM callback);
SCM gmk_s_game_init (SCM title, SCM width, SCM height, SCM fullscreen);
SCM gmk_s_game_run (void);
SCM gmk_s_game_stop (void);
SCM gmk_s_game_pause (void);
SCM gmk_s_game_resume (void);
SCM gmk_s_game_time (void);
SCM gmk_s_game_title (void);
SCM gmk_s_game_window_width (void);
SCM gmk_s_game_window_height (void);
SCM gmk_s_game_resize_window (SCM width, SCM height);
SCM gmk_s_game_fullscreen (void);
SCM gmk_s_set_game_fullscreen (SCM fullscreen);
SCM gmk_s_game_reset_render_image (void);
void gmk_init_game (void);

#endif
