#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

static int debug = 0;
static int robot_debug = 0;

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


// Painting robot

struct position {
	size_t x;
	size_t y;
};

enum direction {
	UP,
	RIGHT,
	DOWN,
	LEFT
};

typedef unsigned char panel;

struct robot {
	struct program *program;
	panel *grid;
	size_t grid_width;
	size_t grid_height;
	struct position pos;
	enum direction dir;
};

size_t grid_position(struct robot *robot) {
	if (robot->pos.x < 0 || robot->grid_width <= robot->pos.x) {
		printf("FAIL! grid is too narrow\n");
		exit(1);
	}
	if (robot->pos.y < 0 || robot->grid_height <= robot->pos.y) {
		printf("FAIL! grid is too short\n");
		exit(1);
	}
	size_t pos = robot->pos.x + robot->pos.y * robot->grid_width;
}

opcode robot_read_camera(void *io_context) {
	struct robot *robot = (struct robot *)io_context;
	int read = robot->grid[grid_position(robot)] & 1;
	if (robot_debug) {
		printf("robot read %u,%u -> %d\n", robot->pos.x, robot->pos.y, read);
	}
	return read;
}

void robot_move(void *io_context, opcode);

void robot_paint(void *io_context, opcode color) {
	struct robot *robot = (struct robot *)io_context;
	size_t pos = grid_position(robot);
	if ((robot->grid[pos] & 1) != color) {
		robot->grid[pos] ^= 1;
	}
	robot->grid[pos] |= 2;

	if (robot_debug) {
		printf("robot write %u,%d <- %u\n", robot->pos.x, robot->pos.y, robot->grid[pos]);
	}

	if (robot->program)
		robot->program->output = robot_move;
}

size_t robot_paint_count(struct robot *robot) {
	size_t total = 0;
	for (size_t i = 0, end = robot->grid_width * robot->grid_height; i < end; ++i) 
		total += robot->grid[i] >> 1;
	return total;
}

void robot_move(void *io_context, opcode turn) {
	struct robot *robot = (struct robot *)io_context;
	switch (turn) {
		case 0:
			robot->dir = (robot->dir == UP) ? LEFT : robot->dir-1;
			break;
		case 1:
			robot->dir = (robot->dir == LEFT) ? UP : robot->dir+1;
			break;
	}
	switch (robot->dir) {
		case UP:
			robot->pos.y--;
			break;
		case RIGHT:
			robot->pos.x++;
			break;
		case DOWN:
			robot->pos.y++;
			break;
		case LEFT:
			robot->pos.x--;
			break;
	}

	if (robot_debug) {
		printf("robot turn %d dir %d pos %d,%d\n", (int)turn, robot->dir, robot->pos.x, robot->pos.y);
	}

	if (robot->program)
		robot->program->output = robot_paint;
}

struct robot *new_robot(struct program *program, size_t width, size_t height) {
	struct robot *robot = (struct robot *)calloc(sizeof(struct robot), 1);
	if (program) {
		robot->program = copy_program(program);
		robot->program->io_context = robot;
		robot->program->input = robot_read_camera;
		robot->program->output = robot_paint;
	}
	robot->grid = (panel *)calloc(width * height, sizeof(panel));
	robot->grid_width = width;
	robot->grid_height = height;
	robot->pos.x = width/2;
	robot->pos.y = height/2;
	robot->dir = UP;
	return robot;
}

void free_robot(struct robot *robot) {
	if (robot->program)
		free_program(robot->program);
	free(robot->grid);
	free(robot);
}

void print_panel_grid(struct robot *robot) {
	// find edges
	struct position top_left = { .x = robot->grid_width, .y = robot->grid_height };
	struct position bottom_right = { .x = 0, .y = 0 };
	for (size_t x = 0; x < robot->grid_width; ++x) {
		for (size_t y = 0; y < robot->grid_height; ++y) {
			if (robot->grid[x + robot->grid_width * y] & 1) {
				top_left.x = x < top_left.x ? x : top_left.x;
				top_left.y = y < top_left.y ? y : top_left.y;
				bottom_right.x = x > bottom_right.x ? x : bottom_right.x;
				bottom_right.y = y > bottom_right.y ? y : bottom_right.y;
			}
		}
	}

	printf("Painted extent %u,%u -> %u,%u\n", top_left.x, top_left.y, bottom_right.x, bottom_right.y);

	for (size_t y = top_left.y; y <= bottom_right.y; ++y) {
		for (size_t x = top_left.x; x <= bottom_right.x; ++x) {
			putchar(robot->grid[x + robot->grid_width * y] & 1 ? '#' : ' ');
		}
		putchar('\n');
	}
}

// tests

void part1_robot_test() {
	struct robot *robot = new_robot(NULL, 6, 6);
	struct position start = robot->pos;
	assert(robot_read_camera(robot) == 0);
	robot_paint(robot, 1);
	robot_move(robot, 0);
	assert(robot_read_camera(robot) == 0);
	robot_paint(robot, 0);
	robot_move(robot, 0);
	assert(robot_read_camera(robot) == 0);
	robot_paint(robot, 1);
	robot_move(robot, 0);
	robot_paint(robot, 1);
	robot_move(robot, 0);
	assert(robot->pos.x == start.x);
	assert(robot->pos.y == start.y);
	assert(robot_read_camera(robot) == 1);
	robot_paint(robot, 0);
	robot_move(robot, 1);
	robot_paint(robot, 1);
	robot_move(robot, 0);
	robot_paint(robot, 1);
	robot_move(robot, 0);
	assert(robot->pos.x == start.x);
	assert(robot->pos.y == start.y - 1);
	assert(robot_read_camera(robot) == 0);
	assert(robot_paint_count(robot) == 6);
	free_robot(robot);
}


// solutions

int part1(struct program *program) {
	printf("Part 1\n");

	struct robot *robot = new_robot(program, 200, 100);

	int result = run_program(robot->program, 0);
	printf("exit:%d; Robot painted %u tiles\n", result, robot_paint_count(robot));

	free_robot(robot);
}


int part2(struct program *program) {
	printf("Part 2\n");

	struct robot *robot = new_robot(program, 200, 100);
	robot->grid[grid_position(robot)] = 1;

	int result = run_program(robot->program, 0);
	printf("exit:%d; Robot painted %u tiles\n", result, robot_paint_count(robot));
	print_panel_grid(robot);

	free_robot(robot);
}


int main(int argc, char **argv) {
	part1_robot_test();

	struct program *program = load_program("input.txt");

	part1(program);
	part2(program);

	free_program(program);

	return 0;
}