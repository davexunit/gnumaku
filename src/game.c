#include "game.h"
#include <time.h>

static ALLEGRO_DISPLAY *display = NULL;
static ALLEGRO_EVENT_QUEUE *event_queue = NULL;
static ALLEGRO_TIMER *timer = NULL;
static char *window_title = NULL;
static float timestep = 1.0f / 60.0f;
static float last_update_time = 0;
static float time_accumulator = 0;
static bool running = false;
static bool paused = false;
static bool redraw = true;
static SCM on_start = SCM_BOOL_F;
static SCM on_update = SCM_BOOL_F;
static SCM on_draw = SCM_BOOL_F;
static SCM on_key_pressed = SCM_BOOL_F;
static SCM on_key_released = SCM_BOOL_F;

static void
game_destroy (void)
{
    al_destroy_timer (timer);
    al_destroy_event_queue (event_queue);
    al_destroy_display (display);
}

static void
game_update (void)
{
    float time = al_get_time();
    float dt = time - last_update_time;

    redraw = true;
    last_update_time = time;

    /* No updates while paused. */
    if (!paused) {
        time_accumulator += dt;

        while (time_accumulator >= timestep) {
            time_accumulator -= timestep;
            if (scm_is_true (on_update)) {
                scm_call_0 (on_update);
            }
        }
    }
}

static void
game_process_event (void)
{
    static ALLEGRO_EVENT event;

    al_wait_for_event(event_queue, &event);

    if (event.type == ALLEGRO_EVENT_DISPLAY_CLOSE) {
        running = false;
    }
    else if (event.type == ALLEGRO_EVENT_TIMER) {
        game_update ();
    }
    else if (event.type == ALLEGRO_EVENT_KEY_UP) {
        if (scm_is_true (on_key_released)) {
            scm_call_1 (on_key_released, scm_from_int (event.keyboard.keycode));
        }
    }
    else if (event.type == ALLEGRO_EVENT_KEY_DOWN) {
        if (scm_is_true (on_key_pressed)) {
            scm_call_1 (on_key_pressed, scm_from_int (event.keyboard.keycode));
        }
    }
}

static void
game_draw (void)
{
    al_clear_to_color (al_map_rgb(0, 0, 0));

    if (scm_is_true (on_draw)) {
        scm_call_0 (on_draw);
    }

    al_flip_display ();
}

SCM_DEFINE (gmk_s_game_on_start_hook, "game-on-start-hook", 1, 0, 0,
            (SCM callback),
            "Set start callback procedure.")
{
    on_start = callback;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_on_update_hook, "game-on-update-hook", 1, 0, 0,
            (SCM callback),
            "Set update callback procedure.")
{
    on_update = callback;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_on_draw_hook, "game-on-draw-hook", 1, 0, 0,
            (SCM callback),
            "Set draw callback procedure.")
{
    on_draw = callback;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_on_key_pressed_hook, "game-on-key-pressed-hook", 1, 0, 0,
            (SCM callback),
            "Set key pressed event callback procedure.")
{
    on_key_pressed = callback;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_on_key_released_hook, "game-on-key-released-hook", 1, 0, 0,
            (SCM callback),
            "Set key released event callback procedure.")
{
    on_key_released = callback;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_init, "game-init", 4, 0, 0,
            (SCM title, SCM width, SCM height, SCM fullscreen),
            "Initialize game subsystems and open window.")
{
    /* Initialize Allegro things */
    /* TODO: Handle these errors in a proper way */
    if (!al_init ()) {
        fprintf (stderr, "failed to initialize allegro!\n");
        exit (-1);
    }

    if (!al_init_primitives_addon ()) {
        fprintf (stderr, "failed to initialize primitives addon!\n");
        exit (-1);
    }

    if (!al_init_image_addon ()) {
        fprintf (stderr, "failed to initialize image addon!\n");
        exit (-1);
    }

    al_init_font_addon ();

    if (!al_init_ttf_addon ()) {
        fprintf (stderr, "failed to initialize ttf addon!\n");
        exit (-1);
    }

    if (!al_install_audio ()) {
        fprintf (stderr, "failed to initialize audio addon!\n");
        exit (-1);
    }

    if (!al_init_acodec_addon ()) {
        fprintf (stderr, "failed to initialize audio codecs addon!\n");
        exit (-1);
    }

    if (!al_reserve_samples (16)) {
        fprintf (stderr, "failed to reserve samples!\n");
        exit (-1);
    }

    if(!al_install_keyboard ()) {
        fprintf (stderr, "failed to initialize keyboard!\n");
        exit (-1);
    }

    if (scm_is_true (fullscreen)) {
        al_set_new_display_flags (ALLEGRO_FULLSCREEN);
    }

    display = al_create_display (scm_to_int (width), scm_to_int (height));

    if (!display) {
        fprintf (stderr, "failed to create display!\n");
    }

    /* Set window title. */
    window_title = scm_to_locale_string (title);
    al_set_window_title (display, window_title);

    /* Register event sources. */
    timer = al_create_timer (timestep);
    event_queue = al_create_event_queue ();
    al_register_event_source (event_queue,
                              al_get_display_event_source (display));
    al_register_event_source (event_queue,
                              al_get_timer_event_source (timer));
    al_register_event_source (event_queue, al_get_keyboard_event_source ());

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_pause, "game-pause", 0, 0, 0,
            (void),
            "Pause game loop. Useful for debugging.")
{
    paused = true;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_resume, "game-resume", 0, 0, 0,
            (void),
            "Resume game loop. Useful for debugging.")
{
    paused = false;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_run, "game-run", 0, 0, 0,
            (void),
            "Enter game loop.")
{
    running = true;

    if (scm_is_true (on_start)) {
        scm_call_0 (on_start);
    }

    al_start_timer (timer);
    last_update_time = al_get_time ();

    while (running) {
        game_process_event ();

        if (redraw && al_is_event_queue_empty (event_queue)) {
            redraw = false;
            game_draw ();
        }
    }

    game_destroy ();

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_time, "game-time", 0, 0, 0,
            (void),
            "Return time elapsed in seconds.")
{
    return scm_from_double (al_get_time ());
}

SCM_DEFINE (gmk_s_game_title, "game-title", 0, 0, 0,
            (void),
            "Return game title.")
{
    return scm_from_latin1_string (window_title);
}

SCM_DEFINE (gmk_s_game_stop, "game-stop", 0, 0, 0,
            (void),
            "Stop game and exit program.")
{
    running = false;

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_window_width, "game-window-width", 0, 0, 0,
            (void),
            "Return display window width.")
{
    return scm_from_int (al_get_display_width (display));
}

SCM_DEFINE (gmk_s_game_window_height, "game-window-height", 0, 0, 0,
            (void),
            "Return display window height")
{
    return scm_from_int (al_get_display_height (display));
}

SCM_DEFINE (gmk_s_game_resize_window, "game-resize-window", 2, 0, 0,
            (SCM width, SCM height),
            "Resize display window.")
{
    al_resize_display (display, scm_to_int (width), scm_to_int (height));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_fullscreen, "game-fullscreen?", 0, 0, 0,
            (void),
            "Return @code{#t} if display window is in fullscreen mode.")
{
    return scm_from_bool (al_get_display_flags (display) &  ALLEGRO_FULLSCREEN);
}

SCM_DEFINE (gmk_s_set_game_fullscreen, "set-game-fullscreen", 1, 0, 0,
            (SCM fullscreen),
            "Set display window fullscreen flag")
{
    al_set_display_flag (display, ALLEGRO_FULLSCREEN, scm_to_bool (fullscreen));

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_s_game_reset_render_image, "game-reset-render-image", 0, 0, 0,
            (void),
            "Reset render image to the display window's image buffer.")
{
    al_set_target_backbuffer (display);

    return SCM_UNSPECIFIED;
}

void
gmk_init_game (void)
{
#include "game.x"

    scm_c_export (s_gmk_s_game_on_start_hook,
                  s_gmk_s_game_on_update_hook,
                  s_gmk_s_game_on_draw_hook,
                  s_gmk_s_game_on_key_pressed_hook,
                  s_gmk_s_game_on_key_released_hook,
                  s_gmk_s_game_init,
                  s_gmk_s_game_run,
                  s_gmk_s_game_stop,
                  s_gmk_s_game_pause,
                  s_gmk_s_game_resume,
                  s_gmk_s_game_time,
                  s_gmk_s_game_title,
                  s_gmk_s_game_window_width,
                  s_gmk_s_game_window_height,
                  s_gmk_s_game_resize_window,
                  s_gmk_s_game_fullscreen,
                  s_gmk_s_set_game_fullscreen,
                  s_gmk_s_game_reset_render_image,
                  NULL);
}
