#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int debug = 0;
static int screen_debug = 1;

// IntCode computer

typedef long opcode;
typedef opcode (* input_function)(void *);
typedef void (* output_function)(void *, opcode);

struct program {
	size_t memory_size;
	size_t pc;
	size_t relative_base;
	input_function input;
	output_function output;
	void *io_context;
	opcode *memory;
};

enum parameter_mode {
	position_mode = 0,
	immediate_mode = 1,
	relative_mode = 2
};

size_t memory_size(size_t size) {
	return size * sizeof(opcode);
}

size_t increase_memory(opcode **memory, size_t old_size) {
	size_t new_size = old_size*2;
	*memory = (opcode *)realloc(*memory, memory_size(new_size));
	memset(*memory + old_size, 0, memory_size(old_size));
	return new_size;
}

struct program *load_program(char *filename) {
	size_t size = 100;
	opcode *memory = (opcode *)malloc(memory_size(size));
	memset(memory, 0, memory_size(size));
	size_t pos = 0;
	opcode cur = 0;
	opcode sign = 1;
	FILE *f = fopen(filename, "rt");
	for (;;) {
		int c = fgetc(f);
		if (c == EOF || c == ',') {
			if (pos == size) {
				size = increase_memory(&memory, size);
			}
			memory[pos++] = cur * sign;
			cur = 0;
			sign = 1;
		} else if (c == '-') {
			sign = -1;
		} else if ('0' <= c && c <= '9') {
			cur = (cur * 10) + (c - '0');
		}
		if (c == EOF) break;
	}
	fclose(f);


	struct program *program = (struct program *)malloc(sizeof(struct program));
	program->memory_size = size;
	program->memory = memory;
	program->pc = 0;
	program->relative_base = 0;
	return program;
}

void free_program(struct program *program) {
	free(program->memory);
	free(program);
}

struct program *copy_program(struct program *program) {
	struct program *copy = (struct program *)malloc(sizeof(struct program));
	copy->memory_size = program->memory_size;
	copy->memory = (opcode *)malloc(memory_size(program->memory_size));
	memcpy(copy->memory, program->memory, memory_size(program->memory_size));
	copy->input = program->input;
	copy->output = program->output;
	copy->io_context = program->io_context;
	copy->pc = 0;
	copy->relative_base = 0;
	return copy;
}

void ensure_program_memory(struct program *program, size_t address) {
	while (address >= program->memory_size) {
		program->memory_size = increase_memory(&program->memory, program->memory_size);
	}
}

enum parameter_mode parameter_mode_from_opcode(opcode x, size_t index) {
	switch (x) {
		case 3:
			return position_mode;
		case 4:
			return position_mode;
		default:
			assert(index >= 1);
			x /= 100;
			while (--index) x /= 10;
			return x % 10;
	}
}

int read_opcode(struct program *program, opcode *x) {
	ensure_program_memory(program, program->pc);
	*x = program->memory[program->pc];
	return 0;
}

int read_parameter(struct program *program, size_t index, opcode *x) {
	ensure_program_memory(program, program->pc + index);

	enum parameter_mode mode = parameter_mode_from_opcode(program->memory[program->pc], index);
	opcode immediate = program->memory[program->pc + index];
	switch (mode) {
		case position_mode: {
			ensure_program_memory(program, immediate);
			if (debug > 10) {
				printf("read index %d position mode (immediate %ld) --> %ld", 
					index, immediate, program->memory[immediate]);
			}
			*x = program->memory[immediate];
			break;
		}
		case immediate_mode: {
			if (debug > 10) printf("read index %d immediate --> %ld\n", index, immediate);
			*x = immediate;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("read index %d relative (immediate %ld relative %u) --> %ld\n", 
					index, immediate, address, program->memory[address]);
			}
			*x = program->memory[address];
			break;
		}
	}

	return 0;
}

int write_program(struct program *program, size_t index, opcode value) {
	ensure_program_memory(program, program->pc + index);

	enum parameter_mode mode = parameter_mode_from_opcode(program->memory[program->pc], index);
	opcode immediate = program->memory[program->pc + index];
	switch (mode) {
		case immediate_mode:
			return 1;

		case position_mode: {
			ensure_program_memory(program, immediate);
			if (debug > 10) {
				printf("write index %d position mode (immediate %d) %d <-- %d\n",
					index, immediate, program->memory[immediate], value);
			}
			program->memory[immediate] = value;
			break;
		}
		case relative_mode: {
			size_t address = program->relative_base + immediate;
			ensure_program_memory(program, address);
			if (debug > 10) {
				printf("write index %d relative mode (immediate %d relative %d) <-- %ld\n",
					index, immediate, address, value);
			}
			program->memory[address] = value;
			break;
		}
	}

	return 0;
}

void print_program(struct program *program) {
	printf("%ld", program->memory[0]);
	for (int i = 1; i < program->memory_size / sizeof(opcode); ++i) {
		printf(",%ld", program->memory[i]);
	}
	printf("\n");
}

int program_has_halted(struct program *program) {
	opcode x;
	if (read_opcode(program, &x) != 0) return 1;
	return x == 99;
}

void debug1(struct program *program, opcode i, char *instr, opcode x) {
	if (debug) {
		printf("%04u %16d %4s %d\n", program->pc, i, instr, x);
	}
}

void debug2(struct program *program, opcode i, char *instr, opcode x, opcode y) {
	if (debug) {
		printf("%04u %16d %4s %d,%d\n", program->pc, i, instr, x, y);
	}
}

int run_program(struct program *program, int cooperative) {
	opcode i;
	while (read_opcode(program, &i) == 0 && i != 99) {
		switch (i % 100) {
			case 1: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 1001;
				if (read_parameter(program, 2, &y) != 0) return 1002;
				debug2(program, i, "add", x, y);
				if (write_program(program, 3, x + y) != 0) return 1004;
				program->pc += 4;
				break;
			}
			case 2: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 2001;
				if (read_parameter(program, 2, &y) != 0) return 2002;
				debug2(program, i, "mul", x, y);
				if (write_program(program, 3, x * y) != 0) return 2004;
				program->pc += 4;
				break;
			}
			case 3: {
				opcode x = program->input(program->io_context);
				debug1(program, i, "in", x);
				if (write_program(program, 1, x) != 0) return 3004;
				program->pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, 1, &x) != 0) return 4001;
				debug1(program, i, "out", x);
				program->output(program->io_context, x);
				program->pc += 2;
				if (cooperative)
					return 0;
				else
					break;
			}
			case 5: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 5001;
				if (read_parameter(program, 2, &y) != 0) return 5002;
				debug2(program, i, "jnz", x, y);
				program->pc = (x != 0) ? y : program->pc + 3;
				break;
			}
			case 6: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 6001;
				if (read_parameter(program, 2, &y) != 0) return 6002;
				debug2(program, i, "jz", x, y);
				program->pc = (x == 0) ? y : program->pc + 3;
				break;
			}
			case 7: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 7001;
				if (read_parameter(program, 2, &y) != 0) return 7002;
				debug2(program, i, "lt", x, y);
				if (write_program(program, 3, (x < y) ? 1 : 0) != 0) return 7004;
				program->pc += 4;
				break;
			}
			case 8: {
				opcode x, y;
				if (read_parameter(program, 1, &x) != 0) return 8001;
				if (read_parameter(program, 2, &y) != 0) return 8002;
				debug2(program, i, "eq", x, y);
				if (write_program(program, 3, (x == y) ? 1 : 0) != 0) return 8004;
				program->pc += 4;
				break;
			}
			case 9: {
				opcode x;
				if (read_parameter(program, 1, &x) != 0) return 9001;
				debug1(program, i, "base", x);
				program->relative_base += x;
				program->pc += 2;
				break;
			}
			default:
				return 1;
		}
	}
	return 0;
}


// Painting screen

enum tile {
	EMPTY = 0,
	WALL = 1,
	BLOCK = 2,
	PADDLE = 3,
	BALL = 4
};

struct screen {
	struct program *program;
	enum tile *tiles;
	size_t width;
	size_t height;
	size_t next_x;
	size_t next_y;
};

void print_screen(struct screen *screen) {
	for (size_t y = 0; y <= screen->height; ++y) {
		for (size_t x = 0; x <= screen->width; ++x) {
			char c = ' ';
			switch (screen->tiles[x + screen->width * y]) {
				case EMPTY:
					c = ' ';
					break;
				case WALL:
					c = '|';
					break;
				case BLOCK:
					c = '#';
					break;
				case PADDLE:
					c = '-';
					break;
				case BALL:
					c = 'o';
					break;
			}
			putchar(c);
		}
		putchar('\n');
	}
}

void screen_set_tile(void *io_context, opcode);
void screen_set_x(void *io_context, opcode);
void screen_set_y(void *io_context, opcode);

void screen_set_x(void *io_context, opcode x) {
	struct screen *screen = (struct screen *)io_context;
	if (screen->width <= x) {
		printf("ERROR! screen too small (x=%u)", x);
		exit(1);
	}

	if (screen_debug) {
		printf("screen set x <-- %u\n", x);
	}

	screen->next_x = x;
	screen->program->output = screen_set_y;

}

void screen_set_y(void *io_context, opcode y) {
	struct screen *screen = (struct screen *)io_context;
	if (screen->height <= y) {
		printf("ERROR! screen too small (y=%u)", y);
		exit(1);
	}

	if (screen_debug) {
		printf("screen set y <-- %u\n", y);
	}

	screen->next_y = y;
	screen->program->output = screen_set_tile;
}

void screen_set_tile(void *io_context, opcode tile) {
	struct screen *screen = (struct screen *)io_context;
	screen->tiles[screen->next_x + screen->next_y * screen->width] = tile;

	if (screen_debug) {
		printf("screen set tile <-- %u\n", tile);
	}

	print_screen(screen);
	screen->program->output = screen_set_x;
}


struct screen *new_screen(struct program *program, size_t width, size_t height) {
	struct screen *screen = (struct screen *)calloc(sizeof(struct screen), 1);
	if (program) {
		screen->program = copy_program(program);
		screen->program->io_context = screen;
		screen->program->input = NULL;
		screen->program->output = screen_set_x;
	}
	screen->tiles = (enum tile *)calloc(width * height, sizeof(enum tile));
	screen->width = width;
	screen->height = height;
	return screen;
}

void free_screen(struct screen *screen) {
	if (screen->program)
		free_program(screen->program);
	free(screen->tiles);
	free(screen);
}



// solutions

int part1(struct program *program) {
	printf("Part 1\n");

	struct screen *screen = new_screen(program, 80, 24);

	int result = run_program(screen->program, 0);
	printf("exit:%d\n", result);

	size_t blocks = 0;
	for (size_t i = 0, end = screen->width * screen->height; i < end; ++i) {
		if (screen->tiles[i] == BLOCK)
			blocks++;
	}
	printf("blocks: %u\n", blocks);

	free_screen(screen);
}



int main(int argc, char **argv) {
	struct program *program = load_program("input.txt");

	part1(program);

	free_program(program);

	return 0;
}