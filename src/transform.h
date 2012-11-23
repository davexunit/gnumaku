#ifndef TRANSFORMATION_H
#define TRANSFORMATION_H

#include <allegro5/allegro.h>
#include <libguile.h>

typedef struct {
    ALLEGRO_TRANSFORM transform;
} Transform;

void init_transform_type ();

#endif
