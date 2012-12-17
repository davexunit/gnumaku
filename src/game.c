#include "game.h"

static scm_t_bits game_tag;

static Game*
check_game (SCM game_smob)
{
    scm_assert_smob_type (game_tag, game_smob);

    return (Game *) SCM_SMOB_DATA (game_smob);
}

static SCM
make_game ()
{
    SCM smob;
    Game *game;

    /* Step 1: Allocate the memory block.
     */
    game = (Game *) scm_gc_malloc (sizeof (Game), "game");

    /* Step 2: Initialize it with straight code.
     */
    game->display = NULL;
    game->event_queue = NULL;
    game->timer = NULL;
    game->timestep = 1.0 / 60.0;
    game->time_accumulator = 0;
    game->last_update_time = 0;
    game->on_start = SCM_BOOL_F;
    game->on_update = SCM_BOOL_F;
    game->on_draw = SCM_BOOL_F;
    game->on_key_pressed = SCM_BOOL_F;
    game->on_key_released = SCM_BOOL_F;
    game->running = true;
    game->redraw = true;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, game_tag, game);

    return smob;
}

static SCM
on_start_hook (SCM game_smob, SCM callback)
{
    Game *game = check_game(game_smob);

    game->on_start = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_update_hook (SCM game_smob, SCM callback)
{
    Game *game = check_game(game_smob);

    game->on_update = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_draw_hook (SCM game_smob, SCM callback)
{
    Game *game = check_game(game_smob);

    game->on_draw = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_key_pressed_hook (SCM game_smob, SCM callback)
{
    Game *game = check_game(game_smob);

    game->on_key_pressed = callback;

    return SCM_UNSPECIFIED;
}

static SCM
on_key_released_hook (SCM game_smob, SCM callback)
{
    Game *game = check_game(game_smob);

    game->on_key_released = callback;

    return SCM_UNSPECIFIED;
}

static SCM
game_init (SCM game_smob, SCM s_width, SCM s_height, SCM s_fullscreen)
{
    Game *game = check_game(game_smob);
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

    if(!al_init_ttf_addon ()) {
        fprintf (stderr, "failed to initialize ttf addon!\n");
        exit (-1);
    }

    if(!al_install_audio ()) {
        fprintf (stderr, "failed to initialize audio addon!\n");
        exit (-1);
    }
 
    if(!al_init_acodec_addon ()) {
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
    
    if (fullscreen) {
        al_set_new_display_flags (ALLEGRO_FULLSCREEN);
    }

    game->display = al_create_display (width, height);
    if (!game->display) {
	fprintf (stderr, "failed to create display!\n");
    }

    game->timer = al_create_timer (game->timestep);
    game->event_queue = al_create_event_queue ();
    al_register_event_source (game->event_queue, al_get_display_event_source (game->display));
    al_register_event_source (game->event_queue, al_get_timer_event_source (game->timer));
    al_register_event_source (game->event_queue, al_get_keyboard_event_source ());

    return SCM_UNSPECIFIED;
}

static void
game_destroy (Game *game)
{
    al_destroy_timer (game->timer);
    al_destroy_event_queue (game->event_queue);
    al_destroy_display (game->display);
}

static void
game_update (Game *game)
{
    float time = al_get_time();
    float dt = time - game->last_update_time;

    game->redraw = true;
    game->last_update_time = time;
    game->time_accumulator += dt;

    while (game->time_accumulator >= game->timestep) {
        game->time_accumulator -= game->timestep;
        if (scm_is_true (game->on_update)) {
            scm_call_0 (game->on_update);
        }
    }
}

static void
game_process_event (Game *game)
{
    static ALLEGRO_EVENT event;

    al_wait_for_event(game->event_queue, &event);

    if (event.type == ALLEGRO_EVENT_DISPLAY_CLOSE)
    {
	game->running = false;
    }
    else if (event.type == ALLEGRO_EVENT_TIMER)
    {
	game_update (game);
    }
    else if (event.type == ALLEGRO_EVENT_KEY_UP)
    {
	if (scm_is_true (game->on_key_released))
	{
	    scm_call_1 (game->on_key_released, scm_from_int (event.keyboard.keycode));
	}
    }
    else if (event.type == ALLEGRO_EVENT_KEY_DOWN)
    {
	if (scm_is_true (game->on_key_pressed))
	{
	    scm_call_1 (game->on_key_pressed, scm_from_int (event.keyboard.keycode));
	}
    }
}

static void
game_draw (Game *game)
{
    al_clear_to_color (al_map_rgb(0, 0, 0));

    if (scm_is_true (game->on_draw))
	scm_call_0 (game->on_draw);

    al_flip_display ();
}

static SCM
game_run (SCM game_smob)
{
    Game *game = check_game (game_smob);

    if (scm_is_true (game->on_start)) {
	scm_call_0 (game->on_start);
    }

    al_start_timer (game->timer);
    game->last_update_time = al_get_time ();

    while (game->running) {
	game_process_event (game);

	if (game->redraw && al_is_event_queue_empty (game->event_queue)) {
	    game->redraw = false;
	    game_draw (game);
	}
    }

    game_destroy (game);

    return SCM_UNSPECIFIED;
}

static SCM
game_get_time (SCM game_smob)
{
    // We don't actually need to use the game struct here.
    check_game (game_smob);

    return scm_from_double (al_get_time ());
}

static SCM
game_stop (SCM game_smob)
{
    Game *game = check_game (game_smob);

    game->running = false;

    return SCM_UNSPECIFIED;
}

static SCM
game_display_width (SCM game_smob)
{
    Game *game = check_game (game_smob);

    return scm_from_int (al_get_display_width (game->display));
}

static SCM
game_display_height (SCM game_smob)
{
    Game *game = check_game (game_smob);

    return scm_from_int (al_get_display_height (game->display));
}

static SCM
game_resize_display (SCM game_smob, SCM s_width, SCM s_height)
{
    Game *game = check_game (game_smob);
    int width = scm_to_int (s_width);
    int height = scm_to_int (s_height);

    al_resize_display (game->display, width, height);

    return SCM_UNSPECIFIED;
}

static SCM
game_fullscreen (SCM game_smob)
{
    Game *game = check_game (game_smob);

    return scm_from_bool (al_get_display_flags (game->display) &  ALLEGRO_FULLSCREEN);
}

static SCM
set_game_fullscreen (SCM game_smob, SCM s_fullscreen)
{
    Game *game = check_game (game_smob);
    bool fullscreen = scm_to_bool (s_fullscreen);

    al_set_display_flag (game->display, ALLEGRO_FULLSCREEN, fullscreen);

    return SCM_UNSPECIFIED;
}

static SCM
reset_draw_target (SCM game_smob)
{
    Game *game = check_game (game_smob);

    al_set_target_backbuffer (game->display);

    return SCM_UNSPECIFIED;
}

static SCM
mark_game (SCM game_smob)
{
    Game *game = (Game *) SCM_SMOB_DATA (game_smob);

    // Mark callbacks
    scm_gc_mark (game->on_start);
    scm_gc_mark (game->on_update);
    scm_gc_mark (game->on_draw);
    scm_gc_mark (game->on_key_pressed);
    scm_gc_mark (game->on_key_released);

    return game->on_draw;
}

static size_t
free_game (SCM game_smob)
{
    Game *game = (Game *) SCM_SMOB_DATA (game_smob);

    scm_gc_free (game, sizeof (Game), "game");

    return 0;
}

static int
print_game (SCM game_smob, SCM port, scm_print_state *pstate)
{
    //Game *game = (Game *) SCM_SMOB_DATA (game_smob);

    scm_puts ("#<Game>", port);

    /* non-zero means success */
    return 1;
}

void
init_game_type (void)
{
    game_tag = scm_make_smob_type ("Game", sizeof (Game));
    scm_set_smob_mark (game_tag, mark_game);
    scm_set_smob_free (game_tag, free_game);
    scm_set_smob_print (game_tag, print_game);

    scm_c_define_gsubr ("make-game", 0, 0, 0, make_game);
    scm_c_define_gsubr ("game-on-start-hook", 2, 0, 0, on_start_hook);
    scm_c_define_gsubr ("game-on-update-hook", 2, 0, 0, on_update_hook);
    scm_c_define_gsubr ("game-on-draw-hook", 2, 0, 0, on_draw_hook);
    scm_c_define_gsubr ("game-on-key-pressed-hook", 2, 0, 0, on_key_pressed_hook);
    scm_c_define_gsubr ("game-on-key-released-hook", 2, 0, 0, on_key_released_hook);
    scm_c_define_gsubr ("game-init", 4, 0, 0, game_init);
    scm_c_define_gsubr ("game-run", 1, 0, 0, game_run);
    scm_c_define_gsubr ("game-stop", 1, 0, 0, game_stop);
    scm_c_define_gsubr ("game-get-time", 1, 0, 0, game_get_time);
    scm_c_define_gsubr ("game-display-width", 1, 0, 0, game_display_width);
    scm_c_define_gsubr ("game-display-height", 1, 0, 0, game_display_height);
    scm_c_define_gsubr ("game-resize-display", 3, 0, 0, game_resize_display);
    scm_c_define_gsubr ("game-fullscreen?", 1, 0, 0, game_fullscreen);
    scm_c_define_gsubr ("set-game-fullscreen", 2, 0, 0, set_game_fullscreen);
    scm_c_define_gsubr ("game-reset-draw-target", 1, 0, 0, reset_draw_target);

    scm_c_export ("make-game", NULL);
    scm_c_export ("game-on-start-hook", NULL);
    scm_c_export ("game-on-update-hook", NULL);
    scm_c_export ("game-on-draw-hook", NULL);
    scm_c_export ("game-on-key-pressed-hook", NULL);
    scm_c_export ("game-on-key-released-hook", NULL);
    scm_c_export ("game-init", NULL);
    scm_c_export ("game-run", NULL);
    scm_c_export ("game-stop", NULL);
    scm_c_export ("game-get-time", NULL);
    scm_c_export ("game-display-width", NULL);
    scm_c_export ("game-display-height", NULL);
    scm_c_export ("game-resize-display", NULL);
    scm_c_export ("game-fullscreen?", NULL);
    scm_c_export ("set-game-fullscreen", NULL);
    scm_c_export ("game-reset-draw-target", NULL);
}
