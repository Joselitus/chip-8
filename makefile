CC = gcc
FLAGS = -lSDL2 -lm

chip8.o: chip8.c
	$(CC) -o $@ chip8.c $(FLAGS)

clean:
	rm chip8.o