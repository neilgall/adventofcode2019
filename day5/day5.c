#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef int opcode;

struct buffer {
	size_t size;
	size_t pos;
	opcode base[];
};

struct program {
	size_t size;
	struct buffer *input;
	struct buffer *output;
	opcode base[];
};

enum parameter_mode {
	position_mode = 0,
	immediate_mode = 1
};

size_t buffer_size(size_t size) {
	return sizeof(struct buffer) + sizeof(opcode) * size;
}

struct buffer *new_buffer(size_t size) {
	struct buffer *b = (struct buffer *)malloc(buffer_size(size));
	b->size = size;
	b->pos = 0;
	return b;
}

struct buffer *write_buffer(struct buffer *buffer, opcode value) {
	if (buffer->pos == buffer->size) {
		buffer->size *= 2;
		buffer = (struct buffer *)realloc(buffer, buffer_size(buffer->size));
	}
	buffer->base[buffer->pos++] = value;
	return buffer;
}

int read_buffer(struct buffer *buffer, opcode *value) {
	if (buffer->pos == buffer->size) {
		return 1;
	}
	*value = buffer->base[buffer->pos++];
	return 0;
}

void reset_buffer_for_reading(struct buffer *buffer) {
	buffer->size = buffer->pos;
	buffer->pos = 0;
}

void print_buffer(struct buffer *buffer) {
	for (size_t i = 0; i < buffer->pos; ++i)
		printf("%d ", buffer->base[i]);
	printf("\n");
}

size_t program_size(size_t n) {
	return sizeof(struct program) + sizeof(opcode) * n;
}

struct program *load_program(char *filename) {
	size_t size = 100;
	struct program *program = (struct program *)malloc(program_size(size));
	size_t pos = 0;
	opcode cur = 0;
	opcode sign = 1;
	FILE *f = fopen(filename, "rt");
	for (;;) {
		int c = fgetc(f);
		if (c == EOF || c == ',') {
			if (pos == size) {
				size *= 2;
				program = (struct program *)realloc(program, program_size(size));
			}
			program->base[pos++] = cur * sign;
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
	program->size = pos;
	program->input = new_buffer(100);
	program->output = new_buffer(100);
	return program;
}

void free_program(struct program *program) {
	free(program->input);
	free(program->output);
	free(program);
}

struct program *copy_program(struct program *program) {
	size_t size = program_size(program->size);
	struct program *copy = (struct program *)malloc(size);
	memcpy(copy, program, size);
	copy->input = new_buffer(100);
	copy->output = new_buffer(100);
	return copy;
}

enum parameter_mode parameter_mode_from_opcode(opcode x, size_t index) {
	switch (x) {
		case 3:
			return immediate_mode;
		case 4:
			return position_mode;
		default:
			if (index == 3) 
				return immediate_mode;
			else {
				assert(index >= 1);
				x /= 100;
				while (--index) x /= 10;
				return (x % 10 == 0) ? position_mode : immediate_mode;
			}
	}
}

int read_opcode(struct program *program, size_t pc, opcode *x) {
	if (pc >= program->size) {
		return 1;
	}
	*x = program->base[pc];
	return 0;
}

int read_parameter(struct program *program, size_t pc, size_t index, opcode *x) {
	if (pc + index >= program->size) {
		return 1;
	}
	enum parameter_mode mode = parameter_mode_from_opcode(program->base[pc], index);
	opcode immediate = program->base[pc + index];
	switch (mode) {
		case position_mode:
			if (immediate >= program->size) {
				return 1;
			}
			*x = program->base[immediate];
			break;

		case immediate_mode:
			*x = immediate;
			break;
	}

	return 0;
}

int write_program(struct program *program, size_t pos, opcode value) {
	if (pos >= program->size) {
		return 1;
	}
	program->base[pos] = value;
	return 0;
}

int write_output(struct program *program, opcode value) {
	program->output = write_buffer(program->output, value);
	return 0;
}

void print_program(struct program *program) {
	printf("%u", program->base[0]);
	for (int i = 1; i < program->size; ++i) {
		printf(",%d", program->base[i]);
	}
	printf("\n");
}

int run_program(struct program *program, int debug) {
	size_t pc = 0;
	opcode i;
	while (read_opcode(program, pc, &i) == 0 && i != 99) {
		switch (i % 100) {
			case 1: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 1001;
				if (read_parameter(program, pc, 2, &y) != 0) return 1002;
				if (read_parameter(program, pc, 3, &z) != 0) return 1003;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x + y) != 0) return 1004;
				pc += 4;
				break;
			}
			case 2: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 2001;
				if (read_parameter(program, pc, 2, &y) != 0) return 2002;
				if (read_parameter(program, pc, 3, &z) != 0) return 2003;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x * y) != 0) return 2004;
				pc += 4;
				break;
			}
			case 3: {
				opcode x, z;
				if (read_buffer(program->input, &x) != 0) return 3001;
				if (read_parameter(program, pc, 1, &z) != 0) return 3002;
				if (debug) printf("pc=%u i=%d x=%d z=%d\n", pc, i, x, z);
				if (write_program(program, z, x) != 0) return 3004;
				pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, pc, 1, &x) != 0) return 4001;
				if (debug) printf("pc=%u i=%d x=%d\n", pc, i, x);
				if (write_output(program, x) != 0) return 4004;
				pc += 2;
				break;
			}
			case 5: {
				opcode x, y;
				if (read_parameter(program, pc, 1, &x) != 0) return 5001;
				if (read_parameter(program, pc, 2, &y) != 0) return 5002;
				pc = (x != 0) ? y : pc + 3;
				break;
			}
			case 6: {
				opcode x, y;
				if (read_parameter(program, pc, 1, &x) != 0) return 6001;
				if (read_parameter(program, pc, 2, &y) != 0) return 6002;
				pc = (x == 0) ? y : pc + 3;
				break;
			}
			case 7: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 7001;
				if (read_parameter(program, pc, 2, &y) != 0) return 7002;
				if (read_parameter(program, pc, 3, &z) != 0) return 7003;
				if (write_program(program, z, (x < y) ? 1 : 0) != 0) return 7004;
				pc += 4;
				break;
			}
			case 8: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 8001;
				if (read_parameter(program, pc, 2, &y) != 0) return 8002;
				if (read_parameter(program, pc, 3, &z) != 0) return 8003;
				if (write_program(program, z, (x == y) ? 1 : 0) != 0) return 8004;
				pc += 4;
				break;
			}
			default:
				return 1;
		}
	}
	return 0;
}

void part1(struct program *original) {
	printf("Part 1\n");
	struct program *program = copy_program(original);
	write_buffer(program->input, 1);
	reset_buffer_for_reading(program->input);

	int result = run_program(program, 0);
	printf("Program exit with code %d\n", result);

	print_buffer(program->output);
	free_program(program);
}

void part2(struct program *original) {
	printf("Part 2\n");
	struct program *program = copy_program(original);
	write_buffer(program->input, 5);
	reset_buffer_for_reading(program->input);

	int result = run_program(program, 0);
	printf("Program exit with code %d\n", result);

	print_buffer(program->output);
	free_program(program);
}

static struct program test_program = {
	.size = 48,
	.base = { 
		3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
		1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
		999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99
	}
};

int main(int argc, char **argv) {
	struct program *program = (argc < 2) 
				? load_program("input.txt")
				: copy_program(&test_program);
	part1(program);
	part2(program);

	free_program(program);
	return 0;
}