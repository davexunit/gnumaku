CC = gcc
CFLAGS = -std=c99 -Wall -pedantic -O2 -ggdb -D_GNU_SOURCE `pkg-config --cflags allegro-5.0 allegro_image-5.0 allegro_font-5.0 allegro_ttf-5.0  guile-2.0`
LIBS = `pkg-config --libs allegro-5.0 allegro_image-5.0 allegro_font-5.0 allegro_ttf-5.0 allegro_primitives-5.0 guile-2.0` -lpthread -lm
SOURCES = src/main.c src/math.c src/rect.c src/font.c src/sprite_sheet.c src/sprite.c src/bullet_system.c src/game.c
OBJECTS = $(SOURCES:.c=.o)
EXECUTABLE = gnumaku

.PHONY: clean build run

build: $(SOURCES) $(EXECUTABLE)

clean:
	rm -f $(EXECUTABLE) $(OBJECTS)

run: $(EXECUTABLE)
	./$(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LIBS)

.c.o:
	$(CC) -c $< -o $@ $(CFLAGS)
