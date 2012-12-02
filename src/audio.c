#include "audio.h"

static scm_t_bits sample_tag;

Sample*
check_sample (SCM sample_smob)
{
    scm_assert_smob_type (sample_tag, sample_smob);

    return (Sample *) SCM_SMOB_DATA (sample_smob);
}

static SCM
load_sample (SCM s_file)
{
    SCM smob;
    const char *file = scm_to_locale_string (s_file);
    Sample *sample = (Sample *) scm_gc_malloc (sizeof (Sample), "sample");

    sample->sample = NULL;

    SCM_NEWSMOB (smob, sample_tag, sample);

    sample->sample = al_load_sample (file);

    if (!sample->sample) {
        fprintf (stderr, "failed to load audio sample: %s\n", file);
    }

    return smob;
}

static SCM
play_sample (SCM sample_smob, SCM s_gain, SCM s_pan, SCM s_speed)
{
    Sample *sample = check_sample (sample_smob);
    float gain = scm_to_double (s_gain);
    float pan = scm_to_double (s_pan);
    float speed = scm_to_double (s_speed);

    al_play_sample (sample->sample, gain, pan, speed, ALLEGRO_PLAYMODE_ONCE, NULL);

    return SCM_UNSPECIFIED;
}

static size_t
free_sample (SCM sample_smob)
{
    Sample *sample = (Sample *) SCM_SMOB_DATA (sample_smob);

    al_destroy_sample (sample->sample);

    scm_gc_free (sample, sizeof (Sample), "sample");

    return 0;
}

static int
print_sample (SCM sample_smob, SCM port, scm_print_state *pstate)
{
    // Sample *sample = (Sample *) SCM_SMOB_DATA (sample_smob);

    scm_puts ("#<Sample >", port);

    /* non-zero means success */
    return 1;
}

void
init_sample_type (void)
{
    sample_tag = scm_make_smob_type ("Sample", sizeof (Sample));
    scm_set_smob_mark (sample_tag, 0);
    scm_set_smob_free (sample_tag, free_sample);
    scm_set_smob_print (sample_tag, print_sample);

    scm_c_define_gsubr ("load-sample", 1, 0, 0, load_sample);
    scm_c_define_gsubr ("play-sample", 4, 0, 0, play_sample);

    scm_c_export ("load-sample", NULL);
    scm_c_export ("play-sample", NULL);
}
