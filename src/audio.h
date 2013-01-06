#ifndef GMK_AUDIO_H
#define GMK_AUDIO_H

#include "common.h"

typedef struct {
    ALLEGRO_SAMPLE *sample;
} GmkSample;

typedef struct {
    ALLEGRO_AUDIO_STREAM *stream;
} GmkAudioStream;

void gmk_init_sample (void);
void gmk_init_audio_stream (void);

#endif
