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
    game->font = NULL;
    game->event_queue = NULL;
    game->timer = NULL;
    game->timestep = 1.0 / 60.0;
    game->on_start = SCM_BOOL_F;
    game->on_update = SCM_BOOL_F;
    game->on_draw = SCM_BOOL_F;
    game->on_key_pressed = SCM_BOOL_F;
    game->on_key_released = SCM_BOOL_F;
    game->running = true;
     
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

static void
game_init (Game *game)
{
    /* Initialize Allegro things */
    if(!al_init())
    {
	fprintf (stderr, "failed to initialize allegro!\n");
    }

    if (!al_init_image_addon ())
    {
	fprintf (stderr, "failed to initialize image addon!\n");
    }

    al_init_font_addon ();
    al_init_ttf_addon ();

    if(!al_install_keyboard ()) {
	fprintf (stderr, "failed to initialize keyboard!\n");
    }

    game->display = al_create_display (800, 600);
    if (!game->display) {
	fprintf (stderr, "failed to create display!\n");
    }

    /* Temporary hard-coded font for showing FPS */
    game->font = al_load_ttf_font ("data/fonts/CarroisGothic-Regular.ttf", 24, 0);

    if (!game->font) {
	fprintf(stderr, "failed to load font\n");
    }

    game->timer = al_create_timer (game->timestep);
    game->event_queue = al_create_event_queue ();
    al_register_event_source (game->event_queue, al_get_display_event_source (game->display));
    al_register_event_source (game->event_queue, al_get_timer_event_source (game->timer));
    al_register_event_source (game->event_queue, al_get_keyboard_event_source ());
}

static void
game_destroy (Game *game)
{
    al_destroy_font (game->font);
    al_destroy_timer (game->timer);
    al_destroy_event_queue (game->event_queue);
    al_destroy_display (game->display);
}

static void
game_process_event (Game *game, ALLEGRO_EVENT event)
{

}

static void
game_update (Game *game)
{
    
}

static void
game_draw (Game *game, const char *fps_string)
{
    al_clear_to_color (al_map_rgb(0, 0, 0));

    if (scm_is_true (game->on_draw))
	scm_call_0 (game->on_draw);

    al_draw_text (game->font, al_map_rgb (255, 255, 255), 790, 570, ALLEGRO_ALIGN_RIGHT, fps_string);

    al_flip_display ();
}

/* TODO: Refactor this into a few smaller, easier to follow functions */
static SCM
game_run(SCM game_smob)
{
    Game *game = check_game (game_smob);
    float last_time = 0;
    float fpsbank = 0;
    char fps_string[16] = "0 fps";
    int frames = 0;
    bool redraw = true;

    game_init (game);

    if (scm_is_true (game->on_start))
	scm_call_0 (game->on_start);

    al_start_timer (game->timer);
    last_time = al_get_time ();

    while(game->running)
    {
	// Handle events
	ALLEGRO_EVENT event;
				
	al_wait_for_event(game->event_queue, &event);

	if (event.type == ALLEGRO_EVENT_DISPLAY_CLOSE)
	{
	    game->running = false;
	}
	else if (event.type == ALLEGRO_EVENT_TIMER)
	{
	    redraw = true;

	    // FPS
	    float time = al_get_time ();
	    fpsbank += time - last_time;
	    last_time = time;
	    if (fpsbank >= 1)
	    {
		sprintf (fps_string, "%d fps", frames);
		frames = 0;
		fpsbank -= 1;
	    }

	    if (scm_is_true (game->on_update))
		scm_call_1 (game->on_update, scm_from_double(game->timestep));
	} else if (event.type == ALLEGRO_EVENT_KEY_UP)
	{
	    if (scm_is_true (game->on_key_released))
	    {
		scm_call_1 (game->on_key_released, scm_from_int (event.keyboard.keycode));
	    }
	} else if (event.type == ALLEGRO_EVENT_KEY_DOWN)
	{
	    if (scm_is_true (game->on_key_pressed))
	    {
		scm_call_1 (game->on_key_pressed, scm_from_int (event.keyboard.keycode));
	    }
	}

	// Draw
	if (redraw && al_is_event_queue_empty (game->event_queue)) {
	    redraw = false;
	    frames += 1;
	    game_draw (game, fps_string);
	}
    }

    game_destroy (game);

    return SCM_UNSPECIFIED;
}

static SCM
game_stop(SCM game_smob)
{
    Game *game = check_game (game_smob);
    
    game->running = false;

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
    scm_c_define_gsubr ("game-run", 1, 0, 0, game_run);
    scm_c_define_gsubr ("game-stop", 1, 0, 0, game_stop);
}
