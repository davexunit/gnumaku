#ifndef AUDIO_H
#define AUDIO_H

#include <allegro5/allegro.h>
#include <allegro5/allegro_audio.h>
#include <allegro5/allegro_acodec.h>
#include <libguile.h>

typedef struct
{
    ALLEGRO_SAMPLE *sample;
} Sample;

void init_sample_type (void);

#endif
