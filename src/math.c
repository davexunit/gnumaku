#include <allegro5/allegro.h>

#include "math.h"

float deg2rad (float degrees) {
    return degrees * ALLEGRO_PI / 180;
}

float rad2deg (float radians) {
    return radians * 180 / ALLEGRO_PI;
}

float
rand1 (void) {
    return (float) rand () / (float) RAND_MAX;
}
