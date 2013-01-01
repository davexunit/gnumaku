#include "game.h"

static ALLEGRO_DISPLAY *display = NULL;
static ALLEGRO_EVENT_QUEUE *event_queue = NULL;
static ALLEGRO_TIMER *timer = NULL;
static ALLEGRO_VOICE *voice = NULL;
static ALLEGRO_MIXER *mixer = NULL;
static char *title = NULL;
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

static SCM
on_start_hook (SCM callback) {
    on_start = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_update_hook (SCM callback) {
    on_update = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_draw_hook (SCM callback) {
    on_draw = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_key_pressed_hook (SCM callback) {
    on_key_pressed = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_key_released_hook (SCM callback) {
    on_key_released = callback;

    return SCM_UNSPECIFIED;
}

static SCM
game_init (SCM s_title, SCM s_width, SCM s_height, SCM s_fullscreen) {
    char *title = scm_to_latin1_string (s_title);
    int width = scm_to_int (s_width);
    int height = scm_to_int (s_height);
    bool fullscreen = scm_to_bool (s_fullscreen);

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

    voice = al_create_voice (44100, ALLEGRO_AUDIO_DEPTH_INT16,
                             ALLEGRO_CHANNEL_CONF_2);

    if (!voice) {
        fprintf (stderr, "failed to create voice!.\n");
        exit (-1);
    }

    mixer = al_create_mixer (44100, ALLEGRO_AUDIO_DEPTH_FLOAT32,
                             ALLEGRO_CHANNEL_CONF_2);

    if (!mixer) {
        fprintf (stderr, "failed to create mixer!\n");
        exit (-1);
    }

    if (!al_attach_mixer_to_voice (mixer, voice)) {
        fprintf (stderr, "failed to attach mixer to voice!\n");
        exit (-1);
    }

    if(!al_install_keyboard ()) {
	fprintf (stderr, "failed to initialize keyboard!\n");
        exit (-1);
    }

    if (fullscreen) {
        al_set_new_display_flags (ALLEGRO_FULLSCREEN);
    }

    display = al_create_display (width, height);

    if (!display) {
	fprintf (stderr, "failed to create display!\n");
    }

    /* Set window title. */
    title = title;
    al_set_window_title (display, title);

    timer = al_create_timer (timestep);
    event_queue = al_create_event_queue ();
    al_register_event_source (event_queue,
                              al_get_display_event_source (display));
    al_register_event_source (event_queue,
                              al_get_timer_event_source (timer));
    al_register_event_source (event_queue, al_get_keyboard_event_source ());

    return SCM_UNSPECIFIED;
}

static void
game_destroy () {
    al_destroy_timer (timer);
    al_destroy_event_queue (event_queue);
    al_destroy_display (display);
}

static void
game_update () {
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
game_process_event () {
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
game_draw () {
    al_clear_to_color (al_map_rgb(0, 0, 0));

    if (scm_is_true (on_draw)) {
	scm_call_0 (on_draw);
    }

    al_flip_display ();
}

static SCM
game_pause () {
    paused = true;

    return SCM_UNSPECIFIED;
}

static SCM
game_resume () {
    paused = false;

    return SCM_UNSPECIFIED;
}

static SCM
game_run () {
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

static SCM
game_time () {
    return scm_from_double (al_get_time ());
}

static SCM
game_title () {
    return scm_from_latin1_string (title);
}

static SCM
game_stop () {
    running = false;

    return SCM_UNSPECIFIED;
}

static SCM
game_display_width () {
    return scm_from_int (al_get_display_width (display));
}

static SCM
game_display_height () {
    return scm_from_int (al_get_display_height (display));
}

static SCM
game_resize_display (SCM s_width, SCM s_height) {
    int width = scm_to_int (s_width);
    int height = scm_to_int (s_height);

    al_resize_display (display, width, height);

    return SCM_UNSPECIFIED;
}

static SCM
game_fullscreen () {
    return scm_from_bool (al_get_display_flags (display) &  ALLEGRO_FULLSCREEN);
}

static SCM
set_game_fullscreen (SCM s_fullscreen) {
    bool fullscreen = scm_to_bool (s_fullscreen);

    al_set_display_flag (display, ALLEGRO_FULLSCREEN, fullscreen);

    return SCM_UNSPECIFIED;
}

static SCM
reset_draw_target () {
    al_set_target_backbuffer (display);

    return SCM_UNSPECIFIED;
}

void
init_game_bindings (void) {
    scm_c_define_gsubr ("game-on-start-hook", 1, 0, 0, on_start_hook);
    scm_c_define_gsubr ("game-on-update-hook", 1, 0, 0, on_update_hook);
    scm_c_define_gsubr ("game-on-draw-hook", 1, 0, 0, on_draw_hook);
    scm_c_define_gsubr ("game-on-key-pressed-hook", 1, 0, 0, on_key_pressed_hook);
    scm_c_define_gsubr ("game-on-key-released-hook", 1, 0, 0, on_key_released_hook);
    scm_c_define_gsubr ("game-init", 4, 0, 0, game_init);
    scm_c_define_gsubr ("game-run", 0, 0, 0, game_run);
    scm_c_define_gsubr ("game-stop", 0, 0, 0, game_stop);
    scm_c_define_gsubr ("game-pause", 0, 0, 0, game_pause);
    scm_c_define_gsubr ("game-resume", 0, 0, 0, game_resume);
    scm_c_define_gsubr ("game-time", 0, 0, 0, game_time);
    scm_c_define_gsubr ("game-title", 0, 0, 0, game_title);
    scm_c_define_gsubr ("game-display-width", 0, 0, 0, game_display_width);
    scm_c_define_gsubr ("game-display-height", 0, 0, 0, game_display_height);
    scm_c_define_gsubr ("game-resize-display", 2, 0, 0, game_resize_display);
    scm_c_define_gsubr ("game-fullscreen?", 0, 0, 0, game_fullscreen);
    scm_c_define_gsubr ("set-game-fullscreen", 1, 0, 0, set_game_fullscreen);
    scm_c_define_gsubr ("game-reset-draw-target", 0, 0, 0, reset_draw_target);

    scm_c_export ("game-on-start-hook", NULL);
    scm_c_export ("game-on-update-hook", NULL);
    scm_c_export ("game-on-draw-hook", NULL);
    scm_c_export ("game-on-key-pressed-hook", NULL);
    scm_c_export ("game-on-key-released-hook", NULL);
    scm_c_export ("game-init", NULL);
    scm_c_export ("game-run", NULL);
    scm_c_export ("game-stop", NULL);
    scm_c_export ("game-pause", NULL);
    scm_c_export ("game-resume", NULL);
    scm_c_export ("game-time", NULL);
    scm_c_export ("game-title", NULL);
    scm_c_export ("game-display-width", NULL);
    scm_c_export ("game-display-height", NULL);
    scm_c_export ("game-resize-display", NULL);
    scm_c_export ("game-fullscreen?", NULL);
    scm_c_export ("set-game-fullscreen", NULL);
    scm_c_export ("game-reset-draw-target", NULL);
}
