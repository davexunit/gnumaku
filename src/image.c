#include "image.h"

static scm_t_bits image_tag;

GmkImage *
gmk_check_image (SCM image)
{
    scm_assert_smob_type (image_tag, image);

    return (GmkImage *) SCM_SMOB_DATA (image);
}

static GmkImage *
malloc_image (void)
{
    return (GmkImage *) scm_gc_malloc (sizeof (GmkImage), "image");
}

ALLEGRO_BITMAP *
gmk_scm_to_bitmap (SCM image)
{
    GmkImage *c_image;

    scm_assert_smob_type (image_tag, image);
    c_image = (GmkImage *) SCM_SMOB_DATA (image);

    return c_image->bitmap;
}

SCM
gmk_scm_from_bitmap (ALLEGRO_BITMAP *bitmap)
{
    SCM smob;
    GmkImage *image = malloc_image ();

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = bitmap;

    return smob;
}

SCM_DEFINE (gmk_load_image, "load-image", 1, 0, 0,
            (SCM filename),
            "Load an image from a file.")
{
    SCM smob;
    GmkImage *image = malloc_image ();

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = al_load_bitmap (scm_to_locale_string (filename));

    if (!image->bitmap) {
        scm_error_scm (scm_from_latin1_symbol ("image-error"), SCM_BOOL_F,
                       scm_from_locale_string ("Failed to load image: ~S"),
                       SCM_EOL, scm_list_1 (filename));
    }

    return smob;
}

SCM_DEFINE (gmk_make_image, "make-image", 2, 0, 0,
            (SCM width, SCM height),
            "Make a new bitmap in memory with dimensions"
            "@var{width} x @var{height}.")
{
    SCM smob;
    GmkImage *image = malloc_image ();

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = al_create_bitmap (scm_to_int (width), scm_to_int (height));

    if (!image->bitmap) {
        scm_error_scm (scm_from_latin1_symbol ("image-error"), SCM_BOOL_F,
                       scm_from_locale_string ("Failed to create bitmap"),
                       SCM_EOL, SCM_BOOL_F);
    }

    return smob;
}

SCM_DEFINE (gmk_image_width, "image-width", 1, 0, 0,
            (SCM image),
            "Return image width.")
{
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image);
    int width =  al_get_bitmap_width (bitmap);

    scm_remember_upto_here_1 (image);

    return scm_from_int (width);
}

SCM_DEFINE (gmk_image_height, "image-height", 1, 0, 0,
            (SCM image),
            "Return image height.")
{
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image);
    int height =  al_get_bitmap_height (bitmap);

    scm_remember_upto_here_1 (image);

    return scm_from_int (height);
}

SCM_DEFINE (gmk_draw_image, "draw-image", 3, 0, 0,
            (SCM image, SCM x, SCM y),
            "Draw image at (@var{x}, @var{y}).")
{
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image);

    al_draw_bitmap (bitmap, scm_to_double (x), scm_to_double (y), 0);

    scm_remember_upto_here_1 (image);

    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gmk_set_render_image, "set-render-image", 1, 0, 0,
            (SCM image),
            "Set the image buffer to render to.")
{
    ALLEGRO_BITMAP *bitmap = gmk_scm_to_bitmap (image);

    al_set_target_bitmap (bitmap);

    return SCM_UNSPECIFIED;
}

static size_t
free_image (SCM image_smob)
{
    GmkImage *image = (GmkImage *) SCM_SMOB_DATA (image_smob);

    /* Do not free sub bitmaps. */
    if (!al_is_sub_bitmap (image->bitmap)) {
        al_destroy_bitmap (image->bitmap);
    }

    scm_gc_free (image, sizeof (GmkImage), "image");

    return 0;
}

static int
print_image (SCM image, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<image width: ", port);
    scm_display (gmk_image_width (image), port);
    scm_puts (" height: ", port);
    scm_display (gmk_image_height (image), port);
    scm_puts (">", port);

    return 1;
}

void
gmk_init_image (void)
{
    image_tag = scm_make_smob_type ("<image>", sizeof (GmkImage));
    scm_set_smob_mark (image_tag, 0);
    scm_set_smob_free (image_tag, free_image);
    scm_set_smob_print (image_tag, print_image);

#include "image.x"

    scm_c_export (s_gmk_load_image,
                  s_gmk_make_image,
                  s_gmk_image_width,
                  s_gmk_image_height,
                  s_gmk_draw_image,
                  s_gmk_set_render_image,
                  NULL);
}
