CC = gcc
CFLAGS = -std=c99 -Wall -g -fPIC `pkg-config --cflags guile-2.0`
LIBS = `pkg-config --libs allegro-5.0 allegro_image-5.0 \
	allegro_font-5.0 allegro_ttf-5.0 allegro_primitives-5.0 \
	allegro_acodec-5.0 allegro_audio-5.0 guile-2.0` -lpthread -lm
SOURCES = $(wildcard src/*.c)
OBJECTS = $(SOURCES:.c=.o)
BIN = gnumaku.so

$(BIN): $(OBJECTS)
	$(CC) -shared $(OBJECTS) -o $@ $(LIBS)

src/%.o: src/%.c
	$(CC) -c $< -o $@ $(CFLAGS)

.PHONY clean:
	rm -f $(BIN) $(OBJECTS)
