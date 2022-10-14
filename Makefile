GRASS=./grass

all: $(GRASS)

$(GRASS): src/grass.c
	gcc -O3 $< -o grass
