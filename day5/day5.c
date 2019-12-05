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

struct buffer *new_buffer(size_t size) {
	struct buffer *b = (struct buffer *)malloc(sizeof(struct buffer) + size);
	b->size = size;
	b->pos = 0;
	return b;
}

struct buffer *write_buffer(struct buffer *buffer, opcode value) {
	if (buffer->pos == buffer->size) {
		buffer->size *= 2;
		buffer = (struct buffer *)realloc(buffer, sizeof(struct buffer) + buffer->size);
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
				printf("opcode %u index %d ", x, index);
				assert(index >= 1);
				x /= 100;
				while (--index) x /= 10;
				printf("-> %d\n", x);
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
	printf("write %d <- %d\n", pos, value);
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
				if (read_parameter(program, pc, 1, &x) != 0) return 1;
				if (read_parameter(program, pc, 2, &y) != 0) return 2;
				if (read_parameter(program, pc, 3, &z) != 0) return 3;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x + y) != 0) return 4;
				pc += 4;
				break;
			}
			case 2: {
				opcode x, y, z;
				if (read_parameter(program, pc, 1, &x) != 0) return 5;
				if (read_parameter(program, pc, 2, &y) != 0) return 6;
				if (read_parameter(program, pc, 3, &z) != 0) return 7;
				if (debug) printf("pc=%u i=%d x=%d y=%d z=%d\n", pc, i, x, y, z);
				if (write_program(program, z, x * y) != 0) return 8;
				pc += 4;
				break;
			}
			case 3: {
				opcode x, z;
				if (read_buffer(program->input, &x) != 0) return 9;
				if (read_parameter(program, pc, 1, &z) != 0) return 10;
				if (debug) printf("pc=%u i=%d x=%d z=%d\n", pc, i, x, z);
				if (write_program(program, z, x) != 0) return 11;
				pc += 2;
				break;
			}
			case 4: {
				opcode x;
				if (read_parameter(program, pc, 1, &x) != 0) return 12;
				if (debug) printf("pc=%u i=%d x=%d\n", pc, i, x);
				if (write_output(program, x) != 0) return 13;
				pc += 2;
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

	print_program(program);

	int result = run_program(program, 1);
	printf("Program exit with code %d\n", result);

	print_buffer(program->output);
	free_program(program);
}

static struct program test_program = {
	.size = 5,
	.base = { 1002,4,3,4,33 }
};

int main(int argc, char **argv) {
	struct program *program = (argc < 2) 
				? load_program("input.txt")
				: copy_program(&test_program);
	part1(program);

	free_program(program);
	return 0;
}