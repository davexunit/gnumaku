#include "image.h"

int get_image_width (Image *image) {
    return al_get_bitmap_width (image->bitmap);
}

int get_image_height (Image *image) {
    return al_get_bitmap_height (image->bitmap);
}

static scm_t_bits image_tag;

Image*
check_image (SCM image_smob) {
    scm_assert_smob_type (image_tag, image_smob);

    return (Image *) SCM_SMOB_DATA (image_smob);
}

static SCM
load_image (SCM s_file) {
    SCM smob;
    const char *file = scm_to_locale_string (s_file);
    Image *image = (Image *) scm_gc_malloc (sizeof (Image), "image");

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = al_load_bitmap (file);

    if (!image->bitmap) {
        fprintf (stderr, "failed to load image: %s\n", file);
    }

    return smob;
}

static SCM
make_image (SCM s_width, SCM s_height) {
    SCM smob;
    int width = scm_to_int (s_width);
    int height = scm_to_int (s_height);
    Image *image = (Image *) scm_gc_malloc (sizeof (Image), "image");

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = al_create_bitmap (width, height);

    return smob;
}

SCM
make_image_from_bitmap (ALLEGRO_BITMAP *bitmap) {
    SCM smob;
    Image *image = (Image *) scm_gc_malloc (sizeof (Image), "image");

    image->bitmap = NULL;

    SCM_NEWSMOB (smob, image_tag, image);

    image->bitmap = bitmap;

    return smob;
}

static SCM
image_width (SCM image_smob) {
    Image *image = check_image (image_smob);

    return scm_from_int (al_get_bitmap_width (image->bitmap));
}

static SCM
image_height (SCM image_smob) {
    Image *image = check_image (image_smob);

    return scm_from_int (al_get_bitmap_height (image->bitmap));
}

static SCM
draw_image (SCM image_smob, SCM s_x, SCM s_y) {
    Image *image = check_image (image_smob);
    float x = scm_to_double (s_x);
    float y = scm_to_double (s_y);

    if (image->bitmap) {
        al_draw_bitmap (image->bitmap, x, y, 0);
    }

    return SCM_UNSPECIFIED;
}

static SCM
set_target_image (SCM image_smob) {
    Image *image = check_image (image_smob);

    al_set_target_bitmap (image->bitmap);

    return SCM_UNSPECIFIED;
}

static size_t
free_image (SCM image_smob) {
    Image *image = (Image *) SCM_SMOB_DATA (image_smob);

    // Do not free sub bitmaps
    if (!al_is_sub_bitmap (image->bitmap)) {
        al_destroy_bitmap (image->bitmap);
    }

    scm_gc_free (image, sizeof (Image), "image");

    return 0;
}

static int
print_image (SCM image_smob, SCM port, scm_print_state *pstate) {
    // Image *image = (Image *) SCM_SMOB_DATA (image_smob);

    scm_puts ("#<image >", port);

    /* non-zero means success */
    return 1;
}

void
init_image_type (void) {
    image_tag = scm_make_smob_type ("<image>", sizeof (Image));
    scm_set_smob_mark (image_tag, 0);
    scm_set_smob_free (image_tag, free_image);
    scm_set_smob_print (image_tag, print_image);

    scm_c_define_gsubr ("load-image", 1, 0, 0, load_image);
    scm_c_define_gsubr ("make-image", 2, 0, 0, make_image);
    scm_c_define_gsubr ("image-width", 1, 0, 0, image_width);
    scm_c_define_gsubr ("image-height", 1, 0, 0, image_height);
    scm_c_define_gsubr ("draw-image", 3, 0, 0, draw_image);
    scm_c_define_gsubr ("set-target-image", 1, 0, 0, set_target_image);

    scm_c_export ("load-image", NULL);
    scm_c_export ("make-image", NULL);
    scm_c_export ("image-width", NULL);
    scm_c_export ("image-height", NULL);
    scm_c_export ("draw-image", NULL);
    scm_c_export ("set-target-image", NULL);
}
