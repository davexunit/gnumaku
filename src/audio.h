#ifndef AUDIO_H
#define AUDIO_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>
#include <libguile.h>

typedef struct {
    ALLEGRO_SAMPLE *sample;
} Sample;

typedef struct {
    ALLEGRO_AUDIO_STREAM *stream;
} AudioStream;

void init_sample_type (void);
void init_audio_stream_type (void);

#endif
