#ifndef GMK_AUDIO_H
#define GMK_AUDIO_H

#include "common.h"

/*
 * GmkSample
 *
 * Simple wrapper around ALLEGRO_SAMPLE.
 * For short sound effects.
 */
typedef struct {
    ALLEGRO_SAMPLE *sample;
} GmkSample;

/*
 * GmkAudioStream
 *
 * Simple wrapper around ALLEGRO_AUDIO_STREAM.
 * For music and audio files longer than a few seconds.
 */
typedef struct {
    ALLEGRO_AUDIO_STREAM *stream;
} GmkAudioStream;

SCM gmk_s_load_sample (SCM filename);
SCM gmk_s_play_sample (SCM sample, SCM gain, SCM pan, SCM speed);
SCM gmk_s_load_audio_stream (SCM filename);
SCM gmk_s_play_audio_stream (SCM audio_stream, SCM gain, SCM pan,
                             SCM speed, SCM loop);
void gmk_init_audio (void);

#endif
