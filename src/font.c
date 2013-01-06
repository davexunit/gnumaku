#include "font.h"
#include "color.h"

static scm_t_bits font_tag;

static GmkFont *
check_font (SCM font_smob)
{
    scm_assert_smob_type (font_tag, font_smob);

    return (GmkFont *) SCM_SMOB_DATA (font_smob);
}

SCM_DEFINE (gmk_s_load_font, "load-font", 2, 0, 0,
            (SCM filename, SCM size),
            "Load font from file.")
{
    SCM smob;
    GmkFont *font;

    font = (GmkFont *) scm_gc_malloc (sizeof (GmkFont), "font");
    font->font = NULL;

    SCM_NEWSMOB (smob, font_tag, font);

    font->font = al_load_ttf_font (scm_to_locale_string (filename),
                                   scm_to_int (size), 0);

    if (!font->font) {
        scm_error_scm (scm_from_latin1_symbol ("font-error"), SCM_BOOL_F,
                       scm_from_locale_string ("Failed to load font: ~S"),
                       SCM_EOL, scm_list_1 (filename));
    }

    return smob;
}

SCM_DEFINE (gmk_s_draw_text, "draw-text", 4, 0, 0,
            (SCM font, SCM position, SCM color, SCM text),
            "Draw @var{text} using @var{font}.")
{
    GmkFont *f = check_font (font);
    GmkVector2 p = gmk_scm_to_vector2 (position);

    al_draw_text (f->font, gmk_color_mult_alpha (gmk_scm_to_color (color)),
                  p.x, p.y, 0, scm_to_locale_string (text));

    return SCM_UNSPECIFIED;
}

static size_t
free_font (SCM font)
{
    GmkFont *f = (GmkFont *) SCM_SMOB_DATA (font);

    al_destroy_font (f->font);
    scm_gc_free (f, sizeof (GmkFont), "font");

    return 0;
}

static int
print_font (SCM font, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<font>", port);

    return 1;
}

void
gmk_init_font (void)
{
    font_tag = scm_make_smob_type ("font", sizeof (GmkFont));
    scm_set_smob_mark (font_tag, 0);
    scm_set_smob_free (font_tag, free_font);
    scm_set_smob_print (font_tag, print_font);

#include "font.x"

    scm_c_export (s_gmk_s_load_font,
                  s_gmk_s_draw_text,
                  NULL);
}
