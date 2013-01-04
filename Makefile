CC = gcc
GUILE_CFLAGS = `pkg-config --cflags guile-2.0`
CFLAGS = -std=c99 -Wall -g -fPIC $(GUILE_CFLAGS)
LIBS = `pkg-config --libs allegro-5.0 allegro_image-5.0 \
	allegro_font-5.0 allegro_ttf-5.0 allegro_primitives-5.0 \
	allegro_acodec-5.0 allegro_audio-5.0 guile-2.0`
SOURCES = $(wildcard src/*.c)
SNARFS = $(SOURCES:.c=.x)
OBJECTS = $(SOURCES:.c=.o)
BIN = gnumaku.so
.SUFFIXES: .x

$(BIN): $(SNARFS) $(OBJECTS)
	$(CC) -shared $(OBJECTS) -o $@ $(LIBS)

src/%.o: src/%.c
	$(CC) -c $< -o $@ $(CFLAGS)

src/%.x: src/%.c
	guile-snarf -o $@ $< $(GUILE_CFLAGS)

.PHONY clean:
	rm -f $(BIN) $(OBJECTS) $(SNARFS)
