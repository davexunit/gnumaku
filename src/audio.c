#include "audio.h"

static scm_t_bits sample_tag;
static scm_t_bits audio_stream_tag;

static GmkSample *
check_sample (SCM sample)
{
    scm_assert_smob_type (sample_tag, sample);

    return (GmkSample *) SCM_SMOB_DATA (sample);
}

SCM_DEFINE (gmk_s_load_sample, "load-sample", 1, 0, 0,
            (SCM filename),
            "Load an audio sample from file.")
{
    SCM smob;
    GmkSample *sample = (GmkSample *) scm_gc_malloc (sizeof (GmkSample), "sample");

    sample->sample = NULL;

    SCM_NEWSMOB (smob, sample_tag, sample);

    sample->sample = al_load_sample (scm_to_locale_string (filename));

    if (!sample->sample) {
        scm_error_scm (scm_from_latin1_symbol ("sample-error"), SCM_BOOL_F,
                       scm_from_locale_string ("Failed to load sample: ~S"),
                       SCM_EOL, scm_list_1 (filename));
    }

    return smob;
}

SCM_DEFINE (gmk_s_play_sample, "play-sample", 4, 0, 0,
            (SCM sample, SCM gain, SCM pan, SCM speed),
            "Play audio sample. @var{gain}, @var{pan}, and @var{speed} are "
            "floating point numbers. A @var{pan} of 0 means to pan center.")
{
    GmkSample *s = check_sample (sample);

    al_play_sample (s->sample, scm_to_double (gain),
                    scm_to_double (pan), scm_to_double (speed),
                    ALLEGRO_PLAYMODE_ONCE, NULL);
    scm_remember_upto_here_1 (sample);

    return SCM_UNSPECIFIED;
}

static size_t
free_sample (SCM sample)
{
    GmkSample *s = (GmkSample *) SCM_SMOB_DATA (sample);

    al_destroy_sample (s->sample);

    scm_gc_free (sample, sizeof (GmkSample), "sample");

    return 0;
}

static int
print_sample (SCM sample, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<sample>", port);

    return 1;
}

static GmkAudioStream *
check_audio_stream (SCM audio_stream)
{
    scm_assert_smob_type (audio_stream_tag, audio_stream);

    return (GmkAudioStream *) SCM_SMOB_DATA (audio_stream);
}

SCM_DEFINE (gmk_s_load_audio_stream, "load-audio-stream", 1, 0, 0,
            (SCM filename),
            "Load an audio stream from file. "
            "Supports common formats such as wav and ogg.")
{
    SCM smob;
    GmkAudioStream *audio_stream =
        (GmkAudioStream *) scm_gc_malloc (sizeof (GmkAudioStream),
                                          "audio_stream");

    audio_stream->stream = NULL;

    SCM_NEWSMOB (smob, audio_stream_tag, audio_stream);

    /* Some magic numbers here taken from the allegro 5 example for streams. */
    audio_stream->stream = al_load_audio_stream (scm_to_locale_string (filename),
                                                 4, 2048);

    if (!audio_stream->stream) {
        scm_error_scm (scm_from_latin1_symbol ("audio-stream-error"), SCM_BOOL_F,
                       scm_from_locale_string ("Failed to load audio stream: ~S"),
                       SCM_EOL, scm_list_1 (filename));

    }

    return smob;
}

static ALLEGRO_PLAYMODE
get_playmode (bool loop)
{
    return loop ? ALLEGRO_PLAYMODE_LOOP : ALLEGRO_PLAYMODE_ONCE;
}

SCM_DEFINE (gmk_s_play_audio_stream, "play-audio-stream", 5, 0, 0,
            (SCM audio_stream, SCM gain, SCM pan, SCM speed, SCM loop),
            "Play audio stream. @var{gain}, @var{pan}, and @var{speed} are "
            "floating point numbers. @var{pan} of 0 is centered, -1 is left, "
            "and 1 is right.")
{
    GmkAudioStream *stream = check_audio_stream (audio_stream);
    ALLEGRO_PLAYMODE playmode = get_playmode (scm_to_bool (loop));

    al_set_audio_stream_gain (stream->stream, scm_to_double (gain));
    al_set_audio_stream_pan (stream->stream, scm_to_double (pan));;
    al_set_audio_stream_speed (stream->stream, scm_to_double (speed));
    al_set_audio_stream_playmode (stream->stream, playmode);
    al_attach_audio_stream_to_mixer (stream->stream, al_get_default_mixer ());
    scm_remember_upto_here_1 (audio_stream);

    return SCM_UNSPECIFIED;
}

static size_t
free_audio_stream (SCM audio_stream)
{
    GmkAudioStream *stream = (GmkAudioStream *) SCM_SMOB_DATA (audio_stream);

    al_destroy_audio_stream (stream->stream);
    scm_gc_free (stream, sizeof (GmkAudioStream), "audio_stream");

    return 0;
}

static int
print_audio_stream (SCM audio_stream, SCM port, scm_print_state *pstate)
{
    scm_puts ("#<audio-stream>", port);

    return 1;
}

void
gmk_init_audio (void)
{
    /* GmkSample */
    sample_tag = scm_make_smob_type ("sample", sizeof (GmkSample));
    scm_set_smob_mark (sample_tag, 0);
    scm_set_smob_free (sample_tag, free_sample);
    scm_set_smob_print (sample_tag, print_sample);

    /* GmkAudioStream */
    audio_stream_tag = scm_make_smob_type ("audio-stream", sizeof (GmkAudioStream));
    scm_set_smob_mark (audio_stream_tag, 0);
    scm_set_smob_free (audio_stream_tag, free_audio_stream);
    scm_set_smob_print (audio_stream_tag, print_audio_stream);

#include "audio.x"

    scm_c_export (s_gmk_s_load_sample,
                  s_gmk_s_play_sample,
                  s_gmk_s_load_audio_stream,
                  s_gmk_s_play_audio_stream,
                  NULL);
}
