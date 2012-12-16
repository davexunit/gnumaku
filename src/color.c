#include "color.h"

SCM
scm_from_color (ALLEGRO_COLOR color) {
    float r, g, b, a;

    al_unmap_rgba_f (color, &r, &g, &b, &a);
    
    return scm_list_4 (scm_from_double (r),
                       scm_from_double (g),
                       scm_from_double (b),
                       scm_from_double (a));
}

ALLEGRO_COLOR
scm_to_color (SCM s_color) {
    float r = scm_to_double (scm_car (s_color));
    float g = scm_to_double (scm_cadr (s_color));
    float b = scm_to_double (scm_caddr (s_color));
    float a = scm_to_double (scm_cadddr (s_color));
    
    return al_map_rgba_f (r, g, b, a);
}
