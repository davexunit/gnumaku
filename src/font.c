#include "font.h"

static scm_t_bits font_tag;

static Font*
check_font (SCM font_smob)
{
    scm_assert_smob_type (font_tag, font_smob);

    return (Font *) SCM_SMOB_DATA (font_smob);
}

static SCM
make_font (SCM s_filename, SCM s_size)
{
    SCM smob;
    Font *font;
    const char *filename = scm_to_locale_string (s_filename);
    int size = scm_to_int (s_size);

    /* Step 1: Allocate the memory block.
     */
    font = (Font *) scm_gc_malloc (sizeof (Font), "font");

    /* Step 2: Initialize it with straight code.
     */
    font->font = NULL;

    /* Step 3: Create the smob.
     */
    SCM_NEWSMOB (smob, font_tag, font);
    font->font = al_load_ttf_font (filename, size, 0);

    return smob;
}

static SCM
font_draw_text (SCM font_smob, SCM s_x, SCM s_y, SCM s_color, SCM s_text)
{
    Font *font = check_font (font_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);
    const char *text = scm_to_locale_string (s_text);

    al_draw_text (font->font, al_map_rgba_f (1, 1, 1, 1), x, y, 0, text);

    return SCM_UNSPECIFIED;
}

static size_t
free_font (SCM font_smob)
{
    Font *font = (Font *) SCM_SMOB_DATA (font_smob);

    al_destroy_font (font->font);
    scm_gc_free (font, sizeof (Font), "font");

    return 0;
}

static int
print_font (SCM font_smob, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<Font>", port);

    /* non-zero means success */
    return 1;
}

void
init_font_type (void)
{
    font_tag = scm_make_smob_type ("Font", sizeof (Font));
    scm_set_smob_mark (font_tag, 0);
    scm_set_smob_free (font_tag, free_font);
    scm_set_smob_print (font_tag, print_font);

    scm_c_define_gsubr ("make-font", 2, 0, 0, make_font);
    scm_c_define_gsubr ("font-draw-text", 5, 0, 0, font_draw_text);
}
